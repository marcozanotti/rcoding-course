# R Coding ----

# Lecture 2: Tidyverse Wrangling ------------------------------------------
# Marco Zanotti

# Goals:
# - Tidyr
# - Dplyr
# - Dbplyr



# Tidyverse ---------------------------------------------------------------

# https://www.tidyverse.org/
# https://www.tidyverse.org/packages/
# https://tidyverse.tidyverse.org/index.html

# The tidyverse is an opinionated collection of R packages designed for 
# data science. All packages share an underlying design philosophy, 
# grammar, and data structures.

library(tidyverse)



# Tidyr -------------------------------------------------------------------

# https://tidyr.tidyverse.org/
# The goal of tidyr is to help you create tidy data. Tidy data is data where:
#   1. Every column is variable.
#   2. Every row is an observation.
#   3. Every cell is a single value.
# Tidy data describes a standard way of storing data that is used wherever 
# possible throughout the tidyverse. If you ensure that your data is tidy, 
# you’ll spend less time fighting with the tools and more time working on your
# analysis. Learn more about tidy data in vignette("tidy-data").

# tidyr functions fall into five main categories:
#   - “Pivotting” which converts between long and wide forms. tidyr 1.0.0 
#     introduces pivot_longer() and pivot_wider(), replacing the older 
#     spread() and gather() functions. See vignette("pivot") for more details
#   - “Rectangling”, which turns deeply nested lists (as from JSON) into 
#     tidy tibbles. See unnest_longer(), unnest_wider(), hoist(), and 
#     vignette("rectangle") for more details
#   - Nesting converts grouped data to a form where each group becomes a 
#     single row containing a nested data frame, and unnesting does the 
#     opposite. See nest(), unnest(), and vignette("nest") for more details
#   - Splitting and combining character columns. Use separate() and extract() 
#     to pull a single character column into multiple columns; use unite() 
#     to combine multiple columns into a single character column
#   - Make implicit missing values explicit with complete(); make explicit 
#     missing values implicit with drop_na(); replace missing values with 
#     next/previous value with fill(), or a known value with replace_na().

# Real datasets can, and often do, violate the three precepts of tidy data in 
# almost every way imaginable. While occasionally you do get a dataset that 
# you can start analyzing immediately, this is the exception, not the rule. 
# Here we go through the five most common problems with messy datasets, 
# along with their remedies:
#   1. Column headers are values, not variable names
#   2. Multiple variables are stored in one column
#   3. Variables are stored in both rows and columns
#   4. Multiple types of observational units are stored in the same table
#   5. A single observational unit is stored in multiple tables.

# Surprisingly, most messy datasets, including types of messiness not 
# explicitly described above, can be tidied with a small set of tools: 
# pivoting (longer and wider) and separating.

library(tidyr)


# * Column Headers --------------------------------------------------------

# A common type of messy dataset is tabular data designed for presentation, 
# where variables form both the rows and columns, and column headers are 
# values, not variable names. While I would call this arrangement messy, 
# in some cases it can be extremely useful. It provides efficient storage
# for completely crossed designs, and it can lead to extremely efficient 
# computation if desired operations can be expressed as matrix operations.

relig_income
relig_income %>% 
  pivot_longer(-religion, names_to = "income", values_to = "frequency")

billboard
names(billboard)
billboard %>% 
  pivot_longer(
    wk1:wk76, 
    names_to = "week", 
    values_to = "rank", 
    values_drop_na = TRUE
  )


# * Multiple Variables ----------------------------------------------------

# After pivoting columns, the key column is sometimes a combination of 
# multiple underlying variable names. Column headers in this format are 
# often separated by a non-alphanumeric character (e.g. ., -, _, :), or 
# have a fixed width format, like in this dataset. separate() makes it 
# easy to split a compound variables into individual variables. You can 
# either pass it a regular expression to split on (the default is to 
# split on non-alphanumeric columns), or a vector of character positions.

# re-create the tuberculosis dataset
# gender & age into column names
tb <- tibble(
  iso2 = c(rep("IT", 5), rep("EN", 5)),
  year = c(1991:1995, 1991:1995),
  m014 = sample.int(20, 10),
  m1525 = c(NA, sample.int(20, 9)),
  m2534 = sample.int(20, 10),
  f014 = sample.int(20, 10),
  f1525 = sample.int(20, 10),
  f2534 = sample.int(20, 10)
)
tb
tb %>% 
  pivot_longer(
    !c(iso2, year), 
    names_to = "name", 
    values_to = "n"
  ) %>% 
  separate(col = name, into = c("sex", "age"), sep = 1) %>% 
  fill(n, .direction = "down")

tb %>% 
  pivot_longer(
    !c(iso2, year), 
    names_to = c("sex", "age"), 
    names_pattern = "(.)(.+)",
    values_to = "n"
  ) %>% 
  drop_na()


# * Variables in Rows and Columns -----------------------------------------

# The most complicated form of messy data occurs when variables are stored 
# in both rows and columns.

# re-create the weather dataset
# it has variables in individual columns (id, year, month), spread across 
# columns (day, d1-d31) and across rows (tmin, tmax) (minimum and maximum 
# temperature).
# This dataset is mostly tidy, but the element column is not a variable; 
# it stores the names of variables. 
# Fixing this requires widening the data: pivot_wider() is inverse of 
# pivot_longer(), pivoting element and value back out across multiple columns
weather <- tibble(
  id = c(rep("X1", 16), rep("X2", 16)),
  year = rep(c(rep(2000, 4), rep(2005, 4), rep(2000, 4), rep(2005, 4)), 2),
  month = rep(c(1, 4, 7, 10), 8),
  element = rep(c(rep("tmin", 8), rep("tmax", 8)), 2),
  d1 = runif(32, min = 0, max = 30),
  d10 = runif(32, min = 0, max = 30),
  d20 = runif(32, min = 0, max = 30),
  d30 = runif(32, min = 0, max = 30)
) %>% 
  arrange(id, year, month)
weather

weather %>% 
  pivot_longer(
    d1:d30, 
    names_to = "day", 
    values_to = "value"
  ) %>% 
  pivot_wider(
    names_from = element,
    values_from = value
  )


# * Multiple Types --------------------------------------------------------

# Datasets often involve values collected at multiple levels, on different 
# types of observational units. During tidying, each type of observational 
# unit should be stored in its own table. This is closely related to the 
# idea of database normalization, where each fact is expressed in only one 
# place. It’s important because otherwise inconsistencies can arise.

# The billboard dataset actually contains observations on two types of 
# observational units: the song and its rank in each week. This manifests 
# itself through the duplication of facts about the song: artist is repeated 
# many times. This dataset needs to be broken down into two pieces: a song 
# dataset which stores artist and song name, and a ranking dataset which 
# gives the rank of the song in each week.

# Normalization is useful for tidying and eliminating inconsistencies. 
# However, there are few data analysis tools that work directly with 
# relational data, so analysis usually also requires denormalization or 
# the merging the datasets back into one table.

billboard_long <- billboard %>% 
  pivot_longer(
    wk1:wk76, 
    names_to = "week", 
    values_to = "rank", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    week = as.integer(stringr::str_remove(week, "wk")),
    date = as.Date(date.entered) + 7 * (week - 1),
    date.entered = NULL
  )
billboard_long

song <- billboard_long %>% 
  distinct(artist, track) %>%
  mutate(song_id = row_number(), .before = everything())
song

rank <- billboard_long %>%
  left_join(song, by = c("artist", "track")) %>%
  select(song_id, date, week, rank)
rank


# * One Type in Multiple Tables -------------------------------------------

# It’s also common to find data values about a single type of observational 
# unit spread out over multiple tables or files. These tables and files are 
# often split up by another variable, so that each represents a single year, 
# person, or location. As long as the format for individual records is 
# consistent, this is an easy problem to fix:
#   - Read the files into a list of tables.
#   - For each table, add a new column that records the original file name 
#     (the file name is often the value of an important variable).
#   - Combine all tables into a single table.



# Dplyr -------------------------------------------------------------------

# https://dplyr.tidyverse.org/

# dplyr is a grammar of data manipulation, providing a consistent set of 
# verbs that help you solve the most common data manipulation challenges:
#   1. mutate() adds new variables that are functions of existing variables
#   2. select() picks variables based on their names.
#   3. filter() picks cases based on their values.
#   4. summarise() reduces multiple values down to a single summary.
#   5. arrange() changes the ordering of the rows.
# These all combine naturally with group_by() which allows you to perform 
# any operation “by group”. You can learn more about them in vignette("dplyr"). 
# As well as these single-table verbs, dplyr also provides a variety of
# two-table verbs, which you can learn about in vignette("two-table").

# When working with data you must:
#   - Figure out what you want to do
#   - Describe those tasks in the form of a computer program
#   - Execute the program
# The dplyr package makes these steps fast and easy:
#   - By constraining your options, it helps you think about your data 
#     manipulation challenges
#   - It provides simple “verbs”, functions that correspond to the most 
#     common data manipulation tasks, to help you translate your thoughts
#     into code
#   - It uses efficient backends, so you spend less time waiting for the 
#     computer
# This section introduces you to dplyr’s basic set of tools, and shows you 
# how to apply them to data frames. dplyr also supports databases via the 
# dbplyr package, once you’ve installed, read vignette("dbplyr") to learn more.

library(dplyr)

# dplyr aims to provide a function for each basic verb of data manipulation. 
# These verbs can be organised into three categories based on the component 
# of the dataset that they work with:  
#   1. Rows:
#      - filter() chooses rows based on column values
#      - slice() chooses rows based on location
#      - arrange() changes the order of the rows
#   2. Columns:
#      - select() changes whether or not a column is included
#      - relocate() changes the order of the columns
#      - rename() changes the name of columns
#      - mutate() changes the values of columns and creates new columns
#   3. Groups:
#      - group_by() performs operations on variables' categories 
#      - summarise() collapses a group into a single row
#   4. Joins:
#      - left_join()
#      - right_join()
#      - inner_join()
#      - full_join()
#      - anti_join()
#   5. Utilities:
#      - distinct()
#      - pull()
#      - bind_rows() / bind_columns()
#      - rowwise()


# * Rows ------------------------------------------------------------------

# filter
starwars %>% filter(skin_color == "light", eye_color == "brown")

starwars %>% filter(skin_color == "light" & eye_color == "brown")

starwars %>% filter(skin_color == "light" | eye_color == "brown")

# slice
starwars %>% slice(5:10)

starwars %>% slice(-1)

starwars %>% slice_head(n = 3)

starwars %>% slice_tail(n = 3)

starwars %>% slice_sample(n = 5)

starwars %>% slice_sample(prop = .2, replace = TRUE)

starwars %>% slice_max(height, n = 3)

starwars %>% slice_min(height, n = 3)

# arrange
starwars %>% arrange(height, mass)

starwars %>% arrange(desc(height))

starwars %>% arrange(name)


# * Columns ---------------------------------------------------------------

# select
starwars %>% select(height, mass, hair_color, skin_color, eye_color, birth_year)

starwars %>% select(height:birth_year)

starwars %>% select(!(height:birth_year))

starwars %>% select(ends_with("color"))

starwars %>% select(starts_with("h"))

starwars %>% select(matches("^h"))

starwars %>% select(contains("_"))

vars <- c("name", "height")
starwars %>% select(all_of(vars), "mass")

starwars %>% select(height:birth_year, everything()) # relocate with select

# relocate
starwars %>% relocate(sex:homeworld, .before = height)

starwars %>% relocate(sex:homeworld, .after = mass)

# rename
starwars %>% rename(home_world = homeworld, person = name)

# mutate
starwars %>% mutate(height_m = height / 100)

starwars %>%
  mutate(height_m = height / 100) %>%
  select(name, height_m, height, everything())

starwars %>% mutate(height_m = height / 100, .before = height) 

starwars %>%
  mutate(
    height_m = height / 100,
    BMI = mass / (height_m ^ 2) # allows direct reference & multiple operations
  ) %>%
  select(name, BMI, everything())

starwars %>%
  transmute( # mutate that keeps only modified columns
    height_m = height / 100,
    BMI = mass / (height_m^2)
  )

starwars %>% mutate(across(where(is.numeric), ~ .x / 100))


# * Groups ----------------------------------------------------------------

# summarise
starwars %>% summarise(mean(height, na.rm = TRUE))

starwars %>%
  summarise(
    mean_height = mean(height, na.rm = TRUE),
    mean_mass = mean(mass, na.rm = TRUE),
    sd_height = sd(height, na.rm = TRUE),
    sd_mass = sd(mass, na.rm = TRUE)
  )

starwars %>% 
  summarise(across(c(height, mass), c(mean, sd), na.rm = TRUE))

starwars %>% 
  summarise(across(c(height, mass), list(mu = mean, sigma = sd), na.rm = TRUE))

starwars %>% 
  summarise(across(c(height, mass), list(mu = mean, sigma = sd), na.rm = TRUE)) %>% 
  pivot_longer(cols = everything()) %>% 
  separate(name, into = c("type", "measure"), sep = "_") %>% 
  pivot_wider(id_cols = type, names_from = measure, values_from = value)

starwars %>% 
  summarise(across(where(is.character), ~ length(unique(.x))))

starwars %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

min_max <- list(
  min = ~ min(.x, na.rm = TRUE), 
  max = ~ max(.x, na.rm = TRUE)
)
starwars %>% summarise(across(where(is.numeric), min_max))

# group_by
starwars %>% group_by(species)
starwars %>% group_by(species) %>% tally(sort = TRUE)

starwars %>% group_by(sex, gender)
starwars %>% group_by(sex, gender) %>% tally(sort = TRUE)

starwars %>% group_by(species) %>% ungroup() %>% tally(sort = TRUE)

# group_by + summarise
starwars %>% 
  group_by(species) %>%
  summarise(n = n(), height = mean(height, na.rm = TRUE))

starwars %>% 
  group_by(sex, gender) %>%
  summarise(n = n(), height = mean(height, na.rm = TRUE))

# group_by + slice
starwars %>% 
  relocate(species) %>% 
  group_by(species) %>%
  slice(1:2)

starwars %>% 
  relocate(sex, gender) %>% 
  group_by(sex, gender) %>%
  slice(1:2)

# group_by + mutate
starwars %>% 
  select(name, homeworld, height, mass) %>% 
  group_by(homeworld) %>% 
  mutate(
    mean_mass = mean(mass, na.rm = TRUE),
    standard_mass = mass - mean(mass, na.rm = TRUE),
    rank = min_rank(height)
  ) %>% 
  ungroup()

# count
starwars %>%
  count(species)

starwars %>%
  group_by(species) %>% 
  summarise(n = n())


# * Joins -----------------------------------------------------------------

# It’s rare that a data analysis involves only a single table of data. 
# In practice, you’ll normally have many tables that contribute to an analysis, 
# and you need flexible tools to combine them. In dplyr, there are three 
# families of verbs that work with two tables at a time:
#   - Mutating joins, which add new variables to one table from matching 
#     rows in another
#   - Filtering joins, which filter observations from one table based on 
#     whether or not they match an observation in the other table
#   - Set operations, which combine the observations in the data sets as 
#     if they were set elements.
# All two-table verbs work similarly. The first two arguments are x and y, 
# and provide the tables to combine. The output is always a new table with 
# the same type as x.

library(nycflights13)
flights
weather
planes
airports

flights_small <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights_small

flights_small %>% left_join(airlines)

flights_small %>% left_join(weather)

flights_small %>% left_join(planes, by = "tailnum")

flights_small %>% left_join(airports, by = c("dest" = "faa"))


# * Utilities -------------------------------------------------------------

# distinct
starwars
starwars %>% distinct()
starwars %>% distinct(sex)
starwars %>% distinct(sex, .keep_all = TRUE)

# pull
starwars[["name"]]
starwars %>% pull(name)

# bind_rows / bind_columns
luke <- starwars %>% 
  select(name, homeworld, species) %>% 
  filter(name == "Luke Skywalker")
luke
obi <- starwars %>% 
  select(name, homeworld, species) %>% 
  filter(name == "Obi-Wan Kenobi")
obi

bind_rows(luke, obi)
bind_cols(luke, obi)


# rowwise
# dplyr, and R in general, are particularly well suited to performing 
# operations over columns, and performing operations over rows is much harder. 
# In this vignette, you’ll learn dplyr’s approach centered around the 
# row-wise data frame created by rowwise().
# There are three common use cases that we discuss in this vignette:
#   - Row-wise aggregates (e.g. compute the mean of x, y, z).
#   - Calling a function multiple times with varying arguments.
#   - Working with list-columns.
# These types of problems are often easily solved with a for loop, but it’s 
# nice to have a solution that fits naturally into a pipeline.
starwars %>% 
  select(name, height, mass) %>% 
  mutate(average = mean(c(height, mass), na.rm = TRUE))

starwars %>% 
  select(name, height, mass) %>% 
  rowwise() %>% 
  mutate(average = mean(c(height, mass), na.rm = TRUE))

starwars %>% 
  select(name, height, mass) %>% 
  summarise(average = mean(c(height, mass), na.rm = TRUE))

starwars %>% 
  select(name, height, mass) %>% 
  rowwise() %>% 
  summarise(average = mean(c(height, mass), na.rm = TRUE))

starwars %>% 
  select(name, height, mass) %>% 
  rowwise(name) %>% 
  summarise(average = mean(c(height, mass), na.rm = TRUE))



# Dbplyr ------------------------------------------------------------------

# dbplyr is the database backend for dplyr. It allows you to use remote 
# database tables as if they are in-memory data frames by automatically 
# converting dplyr code into SQL.
# To learn more about why you might use dbplyr instead of writing SQL, 
# see vignette("sql"). To learn more about the details of the SQL 
# translation, see vignette("translation-verb") and 
# vignette("translation-function").

library(dbplyr, warn.conflicts = TRUE)
library(DBI)

# dbplyr is designed to work with database tables as if they were local 
# data frames. Database connections are coordinated by the DBI package. 
# Learn more at https://dbi.r-dbi.org/

# To demonstrate this first create an in-memory SQLite 
# database and copy over a dataset.
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, mtcars)

# Now you can retrieve a table using tbl()

mtcars_db <- tbl(con, "mtcars")
mtcars_db

# All dplyr calls are evaluated lazily, generating SQL that is only sent 
# to the database when you request the data.

# lazily generates query
summary_query <- mtcars_db %>% 
  group_by(cyl) %>% 
  summarise(mpg = mean(mpg, na.rm = TRUE)) %>% 
  arrange(desc(mpg))

# see query
summary_query %>% show_query()

# execute query and retrieve results
summary_query %>% collect()
res <- summary_query %>% collect()
res


# * Bikes Database --------------------------------------------------------

# Here is the database that we work with throughout flexdashboard tutorials.

con <- DBI::dbConnect(RSQLite::SQLite(), "data/bikes_database.db")

DBI::dbListTables(con)
bikes_tbl <- tbl(con, "bikes")
bikes_tbl
bikeshops_tbl <- tbl(con, "bikeshops")
bikeshops_tbl
orderlines_tbl <- tbl(con, "orderlines")
orderlines_tbl

processed_data_tbl <- orderlines_tbl %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id")) %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  mutate(extended_price = quantity * price) %>%
  collect()

dbDisconnect(con)

processed_data_tbl
glimpse(processed_data_tbl)

processed_data_tbl <- processed_data_tbl %>%    
  mutate(order.date = lubridate::ymd(order.date)) %>%
  separate(location, into = c("city", "state"), sep = ", ") %>%
  group_by(state) %>%
  summarise(total_revenue = sum(extended_price)) %>%
  ungroup() 
processed_data_tbl %>% VieW()
