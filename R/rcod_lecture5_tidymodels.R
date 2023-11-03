# R Coding ----

# Lecture 5: Tidymodels ---------------------------------------------------
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


# * Tune ------------------------------------------------------------------

# https://tune.tidymodels.org/

# The goal of tune is to facilitate hyperparameter tuning for the tidymodels 
# packages. It relies heavily on recipes, parsnip, and dials.


# * Yardstick -------------------------------------------------------------

# https://yardstick.tidymodels.org/

# yardstick is a package to estimate how well models are working using tidy 
# data principles.



# Modelling with Tidymodels -----------------------------------------------

source("R/utils.R")
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


# * Engines ---------------------------------------------------------------

# Linear Regression 
lm_spec <- linear_reg(
  mode = "regression"
) %>%
  set_engine(engine = "lm")

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

# Linear Regression
set.seed(123)
lm_wkfl <- workflow() %>% 
  add_recipe(base_rcp) %>% 
  add_model(lm_spec) %>% 
  fit(data = training(splits))

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

# Linear Regression
lm_wkfl_last <- lm_wkfl %>% 
  last_fit(splits)
lm_wkfl_last %>% pull(.metrics)
lm_wkfl_last %>% pull(.predictions)

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



# Explainable AI ----------------------------------------------------------

library(skimr)
library(DataExplorer)
library(performance)
library(DALEX)
library(modelStudio)


# * Explorative Data Analysis ---------------------------------------------

mpg
skimr::skim(mpg)


# * Linear Regression Model Diagnostics -----------------------------------

mod_lm <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(hwy ~ displ + class, data = mpg)
performance::check_model(mod_lm)

# Plots 1 & 2 analyze the linearity of the residuals (in-sample 
# model error) versus the fitted values. We want to make sure that our 
# model is error is relatively flat.

# Plots 3 & 4 analyze for collinearity and high leverage points.
# * Collinearity is when features are highly correlated, which can throw 
#   off simple regression models (more advanced models use a concept called 
#   regularization and hyperparameter tuning to control for collinearity).
# * High Leverage Points are observations that deviate far from the average. 
#   These can skew the predictions for linear models, and removal or model 
#   adjustment may be necessary to control model performance.

# Plots 5 & 6 analyze for the normality of residuals, which is how the model 
# error is distributed. If the distributions are skewed, this can indicate 
# problems with the model.

# Assesment
# * 1-2: when our model predictions are around 30, our model has larger 
#        error compared to below 30. We may want to inspect these points to 
#        see what could be contributing to the lower predictions than actuals.
# * 3-4: both of the features have low collinearity (green bars). 
#        None of the predictins are outside of the contour lines indicating 
#        we don’t have high leverage points. No model adjustments are necessary.
# * 5-6: several points towards the end of the quantile plot do fall along 
#        the straight-line. This indicates that the model is not predicting 
#        well for these points. 
#        A slight increase in density around 15, which looks to shift the 
#        distribution to the left of zero. This means that the high-error 
#        predictions should be investigated further to see why the model 
#        is far off on this subset of the data.
       
       
# on our model
performance::check_model(lm_wkfl$fit$fit)


# * Machine Learning Explainers -------------------------------------------

# Explainers
# With a predictive model in hand, we are ready to create an explainer. 
# In basic terms, an explainer is a consistent and unified way to explain 
# predictive models. The explainer can accept many different model types like
# Tidymodels, mlr3, H2O or Python Scikit Learn Models, and it returns the 
# explanation results from the model in a consistent format for investigation.

mod_xgb <- boost_tree(
  mode = "regression",
  learn_rate = 0.3
) %>%
  set_engine(engine = "xgboost") %>% 
  fit(hwy ~ ., data = mpg)

explainer <- DALEX::explain(
  model = mod_xgb,
  data = mpg,
  y = mpg$hwy,
  label = "XGBoost"
)
explainer
modelStudio::modelStudio(explainer)

# Feature Importance Plot
# The feature importance plot is a global representation. This means that it 
# looks all of your observations and tells you which features (columns that 
# help you predict) have in-general the most predictive value for your model.

# Break Down Plot
# The Breakdown plot is a local representation that explains one specific 
# observation. The plot then shows a intercept (starting value) and the 
# positive or negative contribution that each feature has to developing 
# the prediction.

# Note: Global Versus Local Explanations
# I can select a different observation, and we get a completely different
# Break Down plot. This is what happens with local explainers. They change 
# telling us different insights by each observation.

# Shapley Values
# Shapley values are a local representation of the feature importance. 
# Instead of being global, the shapley values will change by observation 
# telling you again the contribution.
# The shapley values are related closely to the Breakdown plot, however 
# you may seem slight differences in the feature contributions. The order 
# of the shapley plot is always in the most important magnitude 
# contribution. We also get positive and negative indicating if the 
# feature decreases or increases the prediction.

# Partial Dependence Plot
# The partial dependence plot helps us examine one feature at a time. 
# Above we are only looking at Displacement. The partial dependence is 
# a global representation, meaning it will not change by observation, 
# but rather helps us see how the model predicts over a range of values 
# for the feature being examined.


# on our model
data_tbl <- bikes_data_preproc %>% 
  timetk::tk_augment_timeseries_signature() %>% 
  mutate(index.num = standardize_vec(index.num)) %>% 
  select(-matches("(iso)|(xts)|(day)|(hour)|(minute)|(second)|(hour12)|(am.pm)")) %>% 
  select(-date)
mod <- boost_tree(
  mode = "regression",
  mtry = 20,
  trees = 500,
  min_n = 3,
  tree_depth = 8,
  learn_rate = 0.01,
  loss_reduction = 0.01
) %>%
  set_engine(engine = "xgboost") %>% 
  fit(total_sales ~ ., data = data_tbl)

explainer <- DALEX::explain(
  model = mod,
  data = data_tbl,
  y = data_tbl$total_sales,
  label = "XGBoost"
)
explainer
modelStudio::modelStudio(explainer = explainer)

