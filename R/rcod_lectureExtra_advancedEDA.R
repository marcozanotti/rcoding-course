# R Coding ----

# Lecture Extra: Explorative Data Analysis --------------------------------
# Marco Zanotti

# Goals:
# - janitor
# - skimr
# - dlookr
# - SmartEDA
# - explore
# - DataExplorer
# - radiant

library(tidyverse)

# PROs:
#
# 1) Faster insights with less code for experienced R users. Exploring a fresh new 
# dataset is exciting. Instead of searching for syntax at Stackoverflow, use all 
# your attention searching for interesting patterns in your data, using just a 
# handful easy to remember functions. Your code is easy to understand - even for 
# non R users.
#
# 2) Instant success for new R users. It is said that R has a steep learning curve, 
# especially if you come from a GUI for your statistical analysis. Instead of 
# learning a lot of R syntax before you can explore data, these packages 
# enable you to have instant success. You can start with just one function and 
# learn other R syntax later step by step.
#
# 3) Interactive data exploration (univariate, bivariate, multivariate). A target 
# can be defined (binary / categorical / numerical).
#
# 4) Generate an Automated Report with one line of code. The target can be binary, 
# categorical or numeric.



# janitor -----------------------------------------------------------------

library(readxl)
library(janitor)

roster_raw <- read_excel("data/dirty_data.xlsx")
roster_raw

roster_raw <- roster_raw |> 
  row_to_names(row_number = 1) |> 
  clean_names()
roster_raw

read_excel("data/dirty_data.xlsx", skip = 1, .name_repair = make_clean_names)

roster <- roster_raw |> 
  remove_empty(c("rows", "cols")) |> 
  remove_constant(na.rm = TRUE, quiet = FALSE) |> # remove the column of all "Yes" values
  mutate(
    hire_date = convert_to_date(
      hire_date, # handle the mixed-format dates
      character_fun = lubridate::mdy
    ),
    cert = dplyr::coalesce(certification, certification_2)
  ) |> 
  select(-certification, -certification_2)

roster |> get_dupes(contains("name"))

# tabulation
roster |> tabyl(subject) # one variable

roster |> 
  filter(hire_date > as.Date("1950-01-01")) |> 
  tabyl(employee_status, full_time) # two variables

roster |> 
  tabyl(full_time, subject, employee_status, show_missing_levels = FALSE) # three variables

roster |> 
  tabyl(employee_status, full_time) |>
  adorn_totals("row") |>
  adorn_percentages("row") |>
  adorn_pct_formatting() |>
  adorn_ns() |>
  adorn_title("combined")



# skimr -------------------------------------------------------------------

library(skimr)

skim(iris)
skim(iris) |> summary()



# dlookr ------------------------------------------------------------------

library(nycflights13)
library(dlookr)

flights

diagnose(flights)
diagnose_numeric(flights)
diagnose_category(flights)
diagnose_outlier(flights)

flights |> 
  plot_outlier(
    diagnose_outlier(flights) |>
      filter(outliers_ratio >= 10) |> 
      select(variables) |>  
      unlist()
  )

describe(flights)

flights |> normality(arr_delay)
flights |> plot_normality(arr_delay)

correlate(flights)
flights |> select(arr_delay, dep_delay, distance) |> correlate() |> plot()

flights |> 
  mutate(
    dep_delay_minmax = transform(dep_delay, method = "minmax"),
    arr_delay_minmax = transform(arr_delay, method = "minmax")
  ) |> 
  select(dep_delay, arr_delay, dep_delay_minmax, arr_delay_minmax)

# find_na(flights)
# find_outliers(flights)
# imputate_na(flights, method = "mean")
# imputate_outlier(flights, method = "mean")


# automatic reports
flights |> 
  diagnose_web_report(
    subtitle = "flights", output_dir = "./", output_file = "Diagn.html", theme = "blue"
  )
flights |> 
  diagnose_paged_report(
    subtitle = "flights", output_dir = "./", output_file = "Diagn.pdf", theme = "blue"
  )

flights |> 
  eda_web_report(
    target = "arr_delay", subtitle = "flights",
    output_dir = "./", output_file = "EDA.html", 
    theme = "blue"
  )
flights |> 
  eda_paged_report(
    target = "arr_delay", subtitle = "flights",
    output_dir = "./", output_file = "EDA.pdf", 
    theme = "blue"
  )

flights |> 
  transformation_web_report(
    target = "arr_delay", subtitle = "flights",
    output_dir = "./", output_file = "transform.html", 
    theme = "blue"
  )
flights |> 
  transformation_paged_report(
    target = "arr_delay", subtitle = "flights",
    output_dir = "./", output_file = "transform.pdf", 
    theme = "blue"
  )



# SmartEDA ----------------------------------------------------------------

library(ISLR)
library(SmartEDA)

Carseats <- ISLR::Carseats

ExpData(data = Carseats, type = 1) # overview of the data 
ExpData(data = Carseats, type = 2) # structure of the data  

# numeric variables
ExpNumStat(
  Carseats,
  by = "A",
  gp = NULL,
  Qnt = seq(0, 1, 0.1),
  MesofShape = 2,
  Outlier = TRUE,
  round = 2
)
ExpNumViz(
  Carseats,
  target = "Price",
  type = 2,
  nlim = 25,
  Page = c(2, 2)
)

# categorical variables
ExpCTable(
  Carseats,
  Target = NULL,
  margin = 1,
  clim = 10,
  nlim = 5,
  round = 2,
  bin = NULL,
  per = TRUE
)
ExpCatViz(
  Carseats,
  target = "Urban",
  fname = NULL,
  clim = 10,
  col = NULL,
  margin = 2,
  Page = c(2, 1),
  sample = 2
)

# variable importance
ExpCatStat(
  Carseats,
  Target = "Urban",
  result = "Stat",
  clim = 10,
  nlim = 5,
  bins = 10,
  Pclass = "Yes",
  plot = TRUE,
  top = 10,
  Round = 2
)

# normality
ExpOutQQ(
  Carseats,
  nlim = 10,
  fname = NULL,
  Page = c(2, 2),
  sample = 4
)

# outliers
ExpOutliers(
  Carseats,
  varlist = c("Sales", "CompPrice", "Income"),
  method = "boxplot",
  capping = c(0.1, 0.9)
)


# automatic report
ExpReport(
  Carseats,
  Target = "Urban",
  label = NULL,
  op_file = "test.html",
  op_dir = getwd(),
  sc = 2,
  sn = 2,
  Rc = "Yes"
)



# explore -----------------------------------------------------------------

library(explore)

describe(iris)

explore(iris)

report(iris, output_dir = getwd())
report(iris, output_dir = getwd(), target = "Species")

iris_new <- iris
iris_new$is_versicolor <- ifelse(iris_new$Species == "versicolor", 1, 0)
iris_new$Species <- NULL
iris_new |> explain_tree(target = is_versicolor) # explain target using a decision tree
iris_new |> explain_logreg(target = is_versicolor) # explain target using a logistic regression



# DataExplorer ------------------------------------------------------------

library(DataExplorer)

introduce(airquality)
plot_intro(airquality)

plot_missing(airquality)

plot_bar(diamonds)
plot_bar(diamonds, with = "price")
plot_bar(diamonds, by = "cut")

plot_histogram(diamonds)
plot_density(diamonds)

plot_qq(diamonds)
plot_qq(diamonds, by = "cut")

plot_correlation(diamonds)
plot_boxplot(diamonds, by = "cut")

plot_scatterplot(split_columns(diamonds)$continuous, by = "price", sampled_rows = 1000L)

plot_prcomp(diamonds, maxcat = 5L)


# automatic report
create_report(airquality)
create_report(diamonds, y = "price")



# radiant -----------------------------------------------------------------

library(radiant)
radiant()

