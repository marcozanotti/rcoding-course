# R Coding ----

# Lecture 5: Tidymodels ---------------------------------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - Tidymodels
# - Recipe
# - Engines
# - Modelling



# Tidyverse ---------------------------------------------------------------

# https://www.tidyverse.org/
# https://www.tidyverse.org/packages/
# https://tidyverse.tidyverse.org/index.html

# The tidyverse is an opinionated collection of R packages designed for 
# data science. All packages share an underlying design philosophy, 
# grammar, and data structures.

library(tidyverse)



# Tidymodels --------------------------------------------------------------

# https://www.tidymodels.org/

# Modeling with the tidyverse uses the collection of tidymodels packages
# The tidymodels framework is a collection of packages for modeling and 
# machine learning using tidyverse principles.

library(tidymodels)


# * Parsnip ---------------------------------------------------------------

# https://parsnip.tidymodels.org/
# https://www.tidymodels.org/find/

# The goal of parsnip is to provide a tidy, unified interface to models 
# that can be used to try a range of models without getting bogged down 
# in the syntactical minutiae of the underlying packages.

# One challenge with different modeling functions available in R that do the 
# same thing is that they can have different interfaces and arguments.
# The model syntax can be very different and that the argument names (and 
# formats) are also different. This is a pain if you switch between 
# implementations.

# The goals of parsnip are to:  
# * Separate the definition of a model from its evaluation.  
# * Decouple the model specification from the implementation 
#   (whether the implementation is in R, spark, or something else).  
# * Harmonize argument names (e.g. n.trees, ntrees, trees) so that users 
#   only need to remember a single name. This will help across model types 
#   too so that trees will be the same argument across random forest as 
#   well as boosting or bagging.

# In particular, parsnip dose this by defining:  
# * the type of model is "random forest"  
# * the mode of the model is "regression" or "classification"  
# * the computational engine is the name of the R package.  

# A list of all parsnip models across different CRAN packages can be found 
# at tidymodels.org.


# * Recipes ---------------------------------------------------------------

# https://recipes.tidymodels.org/index.html

# You may consider recipes as an alternative method for creating and 
# preprocessing design matrices (also known as model matrices) that can 
# be used for modeling or visualization.
# With recipes, you can use dplyr-like pipeable sequences of feature 
# engineering steps to get your data ready for modeling.


# * Rsample ---------------------------------------------------------------

# https://rsample.tidymodels.org/
  
# The rsample package provides functions to create different types of 
# resamples and corresponding classes for their analysis. The goal is 
# to have a modular set of methods that can be used for:
# * resampling for estimating the sampling distribution of a statistic
# * estimating model performance using a holdout set  
# 
# The scope of rsample is to provide the basic building blocks for creating 
# and analyzing resamples of a data set, but this package does not include 
# code for modeling or calculating statistics.


# *Tune -------------------------------------------------------------------

# https://tune.tidymodels.org/

# The goal of tune is to facilitate hyperparameter tuning for the tidymodels 
# packages. It relies heavily on recipes, parsnip, and dials.


# * Yardstick -------------------------------------------------------------

# https://yardstick.tidymodels.org/

# yardstick is a package to estimate how well models are working using tidy 
# data principles.



# Modelling with Tidymodels -----------------------------------------------

source("R/packages.R")


# * Data ------------------------------------------------------------------

path_to_data <- "data/bikes_database.db"
con <- DBI::dbConnect(RSQLite::SQLite(), path_to_data)

DBI::dbListTables(con)
bikes_tbl <- tbl(con, "bikes")
bikeshops_tbl <- tbl(con, "bikeshops")
orderlines_tbl <- tbl(con, "orderlines")

bikes_data <- orderlines_tbl %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id")) %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  mutate(extended_price = quantity * price) %>%
  collect()

DBI::dbDisconnect(con)


# * Pre-processing --------------------------------------------------------

bikes_data_preproc <- bikes_data %>%
  mutate(date = floor_date(ymd(order.date), unit = "week")) %>%
  group_by(date) %>%
  summarize(total_sales = sum(extended_price)) %>%
  ungroup() %>% 
  mutate(total_sales = log1p(total_sales)) %>% 
  select(date, total_sales)

bikes_data_preproc %>%
  ggplot(aes(date, total_sales)) +
  geom_line(color = "#2c3e50") +
  geom_point(color = "#2c3e50", size = 0.1) +
  geom_smooth(method = "loess", span = 0.2, size = 1) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "", y = "")

bikes_data_preproc %>% 
  timetk::plot_time_series(.date_var = date, .value = total_sales)

bikes_data_preproc %>% 
  timetk::plot_acf_diagnostics(.date_var = date, .value = total_sales, .lags = 108)


# * Future Data -----------------------------------------------------------

horizon <- 52 # 1 year

future_data <- bikes_data_preproc %>% 
  future_frame(.date_var = date, .length_out = horizon)


# * Trainig / Test Set ----------------------------------------------------

splits <- bikes_data_preproc %>%
  time_series_split(assess = horizon, cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, total_sales)

training(splits)
testing(splits)


# * Recipes ---------------------------------------------------------------

# base recipe with calendar features
base_rcp <- recipe(total_sales ~ ., data = bikes_data_preproc) %>% 
  step_timeseries_signature(date) %>% 
  step_normalize(date_index.num) %>% 
  step_rm(matches("(iso)|(xts)|(day)|(hour)|(minute)|(second)|(hour12)|(am.pm)")) %>% 
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_rm(date)
base_rcp
base_rcp %>% prep() %>% juice() %>% glimpse()


# Engines -----------------------------------------------------------------

# Elastic Net 
elanet_spec <- linear_reg(
  mode = "regression",
  penalty = 0.05,
  mixture = 0.5
) %>%
  set_engine(engine = "glmnet")

# XGBoost
xgb_spec <- boost_tree(
  mode = "regression",
  mtry = 20,
  trees = 500,
  min_n = 3,
  tree_depth = 8,
  learn_rate = 0.01,
  loss_reduction = 0.01
) %>%
  set_engine(engine = "xgboost")


# * Workflows -------------------------------------------------------------

# Elastic Net
set.seed(123)
elanet_wkfl <- workflow() %>% 
  add_recipe(base_rcp) %>% 
  add_model(elanet_spec) %>% 
  fit(data = training(splits))

# XGBoost
set.seed(123)
xgb_wkfl <- workflow() %>% 
  add_recipe(base_rcp) %>% 
  add_model(xgb_spec) %>% 
  fit(data = training(splits)) 


# * Evaluation ------------------------------------------------------------

# Elastic Net
elanet_wkfl_last <- elanet_wkfl %>% 
  last_fit(splits)
elanet_wkfl_last %>% pull(.metrics)

# XGBoost
xgb_wkfl_last <- xgb_wkfl %>% 
  last_fit(splits)
xgb_wkfl_last %>% pull(.metrics)

# elastic net best model


# * Re-fitting ------------------------------------------------------------

# fit best model on whole dataset
set.seed(123)
elanet_wkfl_refit <- workflow() %>% 
  add_recipe(base_rcp) %>% 
  add_model(elanet_spec) %>% 
  fit(data = bikes_data_preproc)


# * Forecast Out-of-Sample ------------------------------------------------

elanet_wkfl_refit %>% 
  predict(new_data = future_data)

prediction_tbl <- elanet_wkfl_refit %>% 
  predict(new_data = future_data) %>%
  bind_cols(future_data) %>%
  rename(total_sales = .pred) %>%
  add_column(key = "Prediction")
prediction_tbl

output_tbl <- bikes_data_preproc %>%
  add_column(key = "Actual") %>%
  relocate(total_sales, .before = date) %>% 
  bind_rows(prediction_tbl) %>%
  mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))

output_tbl %>% 
  timetk::plot_time_series(date, total_sales, .color_var = key)

