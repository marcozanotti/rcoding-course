# R Coding ----

# Lecture 1: Tidyverse Basics ---------------------------------------------
# Marco Zanotti

# Goals:
# - Dive into the tidyverse
# - Standard vs Non-Standard vs Tidy Evaluation
# - Pipe operator
# - Tibble structures
# - Stringr
# - Forcats
# - Lubridate
# - Readr



# Tidyverse ---------------------------------------------------------------

# https://www.tidyverse.org/
# https://www.tidyverse.org/packages/
# https://tidyverse.tidyverse.org/index.html
# https://r4ds.had.co.nz/index.html

# The tidyverse is an opinionated collection of R packages designed for 
# data science. All packages share an underlying design philosophy, 
# grammar, and data structures.

library(tidyverse)


# * Core Tidyverse --------------------------------------------------------

# tibble
# https://tibble.tidyverse.org/
# A tibble, or tbl_df, is a modern reimagining of the data.frame, keeping 
# what time has proven to be effective, and throwing out what is not. 
# Tibbles are data.frames that are lazy and surly: they do less (i.e. they 
# don’t change variable names or types, and don’t do partial matching) and 
# complain more (e.g. when a variable does not exist). This forces you to 
# confront problems earlier, typically leading to cleaner, more expressive 
# code. Tibbles also have an enhanced print() method which makes them easier 
# to use with large datasets containing complex objects.

# readr
# https://readr.tidyverse.org/
# The goal of readr is to provide a fast and friendly way to read rectangular 
# data (like csv, tsv, and fwf). It is designed to flexibly parse many types 
# of data found in the wild, while still cleanly failing when data unexpectedly 
# changes.

# stringr
# https://stringr.tidyverse.org/
# Strings are not glamorous, high-profile components of R, but they do play 
# a big role in many data cleaning and preparation tasks. The stringr package 
# provide a cohesive set of functions designed to make working with strings 
# as easy as possible.
# stringr is built on top of stringi, which uses the ICU C library to provide 
# fast, correct implementations of common string manipulations. stringr 
# focuses on the most important and commonly used string manipulation 
# functions whereas stringi provides a comprehensive set covering almost 
# anything you can imagine. If you find that stringr is missing a function 
# that you need, try looking in stringi. Both packages share similar 
# conventions, so once you’ve mastered stringr, you should find stringi 
# similarly easy to use.

# forcats
# https://forcats.tidyverse.org/
# R uses factors to handle categorical variables, variables that have a fixed 
# and known set of possible values. Factors are also helpful for reordering 
# character vectors to improve display. The goal of the forcats package is to 
# provide a suite of tools that solve common problems with factors, including 
# changing the order of levels or the values.

# tidyr
# https://tidyr.tidyverse.org/
# The goal of tidyr is to help you create tidy data. Tidy data is data where:
#   1. Every column is variable.
#   2. Every row is an observation.
#   3. Every cell is a single value.
# Tidy data describes a standard way of storing data that is used wherever 
# possible throughout the tidyverse. If you ensure that your data is tidy, 
# you’ll spend less time fighting with the tools and more time working on your
# analysis. Learn more about tidy data in vignette("tidy-data").

# dplyr
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

# ggplot2
# https://ggplot2.tidyverse.org/
# ggplot2 is a system for declaratively creating graphics, based on 
# The Grammar of Graphics. You provide the data, tell ggplot2 how to map 
# variables to aesthetics, what graphical primitives to use, and it takes 
# care of the details.

# purrr
# https://purrr.tidyverse.org/
# purrr enhances R’s functional programming (FP) toolkit by providing a 
# complete and consistent set of tools for working with functions and vectors.
# If you’ve never heard of FP before, the best place to start is the family 
# of map() functions which allow you to replace many for loops with code that 
# is both more succinct and easier to read.


# * Import ----------------------------------------------------------------

# As well as readr, for reading flat files, the tidyverse package installs a 
# number of other packages for reading data:
#   - DBI for relational databases
#   - haven for SPSS, Stata, and SAS data
#   - httr for web APIs
#   - readxl for .xls and .xlsx sheets
#   - googlesheets4 for Google Sheets via the Sheets API v4
#   - googledrive for Google Drive files
#   - rvest for web scraping
#   - jsonlite for JSON
#   - xml2 for XML


# * Wrangle ---------------------------------------------------------------

# In addition to tidyr, and dplyr, there are five packages (including stringr 
# and forcats) which are designed to work with specific types of data:
#   - lubridate for dates and date-times
#   - hms for time-of-day values
#   - blob for storing blob (binary) data
# 
# # dplyr backends 
# There are also two packages that allow you to interface with different 
# backends using the same dplyr syntax:
#   - dbplyr allows you to use remote database tables by converting dplyr 
#     code into SQL.
#   - dtplyr provides a data.table backend by automatically translating 
#     to the equivalent, but usually much faster, data.table code


# * Program ---------------------------------------------------------------

# In addition to purrr, which provides very consistent and natural methods 
# for iterating on R objects, there are two additional tidyverse packages 
# that help with general programming challenges:
#   - magrittr provides the pipe, %>% used throughout the tidyverse. 
#     It also provide a number of more specialized piping operators 
#     (like %$% and %<>%) that can be useful in other places
#   - glue provides an alternative to paste() that makes it easier to 
#     combine data and strings


# * Model -----------------------------------------------------------------

# https://www.tidymodels.org/
# Modeling with the tidyverse uses the collection of tidymodels packages
# The tidymodels framework is a collection of packages for modeling and 
# machine learning using tidyverse principles.



# R Code Evaluation Methods -----------------------------------------------

# * Standard Evaluation ---------------------------------------------------

# https://www.brodieg.com/2020/05/05/on-nse/

# When we type a simple expression at the R prompt and hit ENTER, R computes 
# its value (a.k.a. evaluates it):

w <- c("am", "I", "global")
rev(w) # reverse the order of `w`

# The process of evaluation in R is mostly about looking things up rather 
# than computing them. In our example, when we hit ENTER R first looks for 
# the named objects in our expression: rev, and w. Lookups are done through 
# data structures called environments.

# After the lookup rev(w) becomes:

(function(x) UseMethod("rev"))(c("am", "I", "global"))

# rev is replaced by the definition of the function from the base environment, 
# and w by the character vector from the workspace. The workspace, also known 
# as the global environment, is where name -> value mappings created at the R 
# prompt are kept (e.g. w <- c("am", "I", "global")).

# After the initial substitution of rev and w for the function and its 
# argument, R will call (i.e “run”) the function. This means repeating the 
# lookup-and-substitute process on any R expressions contained in the body 
# of the function. Each time a function is called a new environment is created, 
# enclosed by the Function Environment, and with the function 
# parameter -> value mappings as its Frame. This new environment becomes 
# the Evaluation Environment for the expressions in the function body.
# Once the function Evaluation Environment is set up, R can proceed with 
# the lookup-and-substitute process on the body of the function. In this 
# case there is just the one expression UseMethod("rev") in the body.


# * Non-Standard Evaluation -----------------------------------------------

# http://adv-r.had.co.nz/Computing-on-the-language.html
# https://www.brodieg.com/2020/05/05/on-nse/

# R has powerful tools for computing not only on values, but also on the 
# actions that lead to those values. If you’re coming from another programming 
# language, they are one of the most surprising features of R. Consider the 
# following simple snippet of code that plots a sine curve:

values <- seq(0, 2 * pi, length = 100)
sinx <- sin(values)

plot(x = values, y = sinx, type = "l")
plot(x = seq(0, 2 * pi, length = 100), y = sin(values), type = "l")

# Look at the labels on the axes. How did R know that the variable on the x 
# axis is called x and the variable on the y axis is called sinx? In most
# programming languages, you can only access the values of a function’s 
# arguments. In R, you can also access the code used to compute them. 
# This makes it possible to evaluate code in non-standard ways: to use what 
# is known as non-standard evaluation (NSE). 
# NSE is particularly useful for functions when doing interactive data 
# analysis because it can dramatically reduce the amount of typing.


# * Tidy Evaluation -------------------------------------------------------

# Most tidyverse functions use tidy evaluation in some way. Tidy evaluation is a 
# special type of non-standard evaluation used throughout the tidyverse. 
# There are two basic forms:
#   
#   - arrange(), count(), filter(), group_by(), mutate(), and summarise() 
#     use data masking so that you can use data variables as if they were 
#     variables in the environment 
#     (i.e. you write my_variable not df$my_variable)
# 
#   - across(), relocate(), rename(), select(), and pull() use tidy selection 
#     so you can easily choose variables based on their position, name, 
#     or type (e.g. starts_with("x") or is.numeric)
# 
# To determine whether a function argument uses data masking or tidy selection, 
# look at the documentation: in the arguments list, you’ll see 
# <data-masking> or <tidy-select>.
# 
# Data masking and tidy selection make interactive data exploration fast 
# and fluid, but they add some new challenges when you attempt to use them 
# indirectly such as in a for loop or a function.


# * 1. Data Masking -------------------------------------------------------
# Data masking makes data manipulation faster because it requires less typing. 
# In most (but not all) base R functions you need to refer to variables with $, 
# leading to code that repeats the name of the data frame many times:

starwars
starwars$homeworld == "Naboo" & starwars$species == "Human"
starwars[starwars$homeworld == "Naboo" & starwars$species == "Human", ]
 
# The dplyr equivalent of this code is more concise because data masking allows 
# you to need to type starwars once:
 
filter(starwars, homeworld == "Naboo" & species == "Human")

# Data- and env-variables
# The key idea behind data masking is that it blurs the line between the two 
# different meanings of the word “variable”:
#
#   - env-variables are “programming” variables that live in an environment. 
#     They are usually created with <-
# 
#   - data-variables are “statistical” variables that live in a data frame. 
#     They usually come from data files (e.g. .csv, .xls), or are created 
#     manipulating existing variables.
# 
# To make those definitions a little more concrete, take this piece of code:
   
df <- data.frame(x = runif(3), y = runif(3))
df$x
 
# It creates a env-variable, df, that contains two data-variables, x and y. 
# Then it extracts the data-variable x out of the env-variable df using $.


# * 2. Tidy Select --------------------------------------------------------
# Data masking makes it easy to compute on values within a dataset. 
# Tidy selection is a complementary tool that makes it easy to work with 
# the columns of a dataset. This provides a miniature domain specific language 
# that makes it easy to select columns by name, position, or type. For example:
  
select(df, 1) # selects the first column 
select(df, last_col()) # selects the last column
select(df, x) # selects columns x, and y
select(df, c(x, y)) # selects columns x, and y
select(df, starts_with("x")) # selects all columns whose name starts with “x”
select(df, ends_with("y")) # selects all columns whose name ends with “y”
select(df, where(is.numeric)) # selects all numeric columns



# Pipe Operator -----------------------------------------------------------

# https://magrittr.tidyverse.org/

# The magrittr package offers a set of operators which make your code more 
# readable by:
#   
#   - structuring sequences of data operations left-to-right 
#     (as opposed to from the inside and out)
#   - avoiding nested function calls
#   - minimizing the need for local variables and function definitions
#   - making it easy to add steps anywhere in the sequence of operations
# 
# The operators pipe their left-hand side values forward into expressions 
# that appear on the right-hand side, i.e. one can replace f(x) with x %>% f(), 
# where %>% is the (main) pipe-operator. When coupling several function 
# calls with the pipe-operator, the benefit will become more apparent. 

library(magrittr)

# Consider this example:
 
the_data <- starwars %>%
  filter(height > 200) %>%
  mutate(mass_lbs = mass * 2.205) %>% # convert kg to lbs
  select(name, height, mass, mass_lbs)
the_data

# Four operations are performed to arrive at the desired data set, 
# and they are written in a natural order: the same as the order of execution. 
# Also, no temporary variables are needed. If yet another operation is required, 
# it is straightforward to add to the sequence of operations wherever 
# it may be needed.

# Without piping this results into:
the_data <- 
  select(
    mutate(
      filter(starwars, height > 200),
      mass_lbs = mass * 2.205
    ),
    name, height, mass, mass_lbs
  )

# or
the_data <- filter(starwars, height > 200) 
the_data <- mutate(the_data, mass_lbs = mass * 2.205) # convert kg to lbs
the_data <- select(the_data, name, height, mass, mass_lbs)

# CTRL + MAIUSC + M is the RStudio shortcut for the %>%
# Since R version 4.1, the native pipe operator |> has been introduced  
# BE CAREFUL: The magrittr %>% and the native |> are not exactly the same !!!!!!!!!!!
# https://www.tidyverse.org/blog/2023/04/base-vs-magrittr-pipe/#:~:text=%25%3E%25%20allows%20you%20to%20drop,%7C%3E%20always%20requires%20the%20parentheses.


# * Basic Piping ----------------------------------------------------------

x %>% f() is equivalent to f(x)
x %>% f(y) is equivalent to f(x, y)
x %>% f() %>% g() %>% h() is equivalent to h(g(f(x)))

# “Equivalent” is not technically exact: evaluation is non-standard, 
# and the left-hand side is evaluated before passed on to the right-hand 
# side expression. However, in most cases this has no practical implication.


# * Argument Placeholder --------------------------------------------------

x %>% f(y, .) is equivalent to f(y, x)
x %>% f(y, z = .) is equivalent to f(y, z = x)

# The . has a placeholder function when coupled with the pipe.


# * Re-using Placeholder for Attributes -----------------------------------

# It is straightforward to use the placeholder several times in a 
# right-hand side expression. However, when the placeholder only appears 
# in a nested expressions magrittr will still apply the first-argument rule. 
# The reason is that in most cases this results more clean code.

x %>% f(y = nrow(.), z = ncol(.)) is equivalent to f(x, y = nrow(x), z = ncol(x))
# this behavior can be overruled by enclosing the right-hand side in braces:
x %>% {f(y = nrow(.), z = ncol(.))} is equivalent to f(y = nrow(x), z = ncol(x))



# Tibble ------------------------------------------------------------------

# https://tibble.tidyverse.org/

# A tibble, or tbl_df, is a modern reimagining of the data.frame, keeping 
# what time has proven to be effective, and throwing out what is not. 
# Tibbles are data.frames that are lazy and surly: they do less (i.e. they 
# don’t change variable names or types, and don’t do partial matching) and 
# complain more (e.g. when a variable does not exist). This forces you to 
# confront problems earlier, typically leading to cleaner, more expressive 
# code. Tibbles also have an enhanced print() method which makes them easier 
# to use with large datasets containing complex objects.

library(tibble)

# Create a tibble from an existing object with as_tibble():
df <- data.frame(a = 1:3, b = letters[1:3], c = Sys.Date() - 1:3)
df
df <- data.frame(a = 1:1000)
df
str(df)

tbl <- as_tibble(df)
tbl
str(tbl)
# This will work for reasonable inputs that are already data.frames, 
# lists, matrices, or tables.

# You can also create a new tibble from column vectors with tibble():
tibble(x = 1:5, y = 1, z = x ^ 2 + y)

# tibble() does much less than data.frame(): it never changes the type 
# of the inputs (e.g. it never converts strings to factors!), it never 
# changes the names of variables, it only recycles inputs of length 1, 
# and it never creates row.names(). 
# You can read more about these features in vignette("tibble").

# You can define a tibble row-by-row with tribble():
tribble(
  ~ x, ~ y,  ~ z,
  "a", 2,  3.6,
  "b", 1,  8.5
)

# Useful
m <- matrix(1:9, nrow = 3, ncol = 3)
rownames(m) <- c("a", "b", "c")
colnames(m) <- c("x", "y", "z")
m
m %>% as_tibble(rownames = "names") # convert matrix to tibble



# Stringr -----------------------------------------------------------------

# https://stringr.tidyverse.org/
# https://regexone.com/
# https://www.garrickadenbuie.com/project/regexplain/

# Strings are not glamorous, high-profile components of R, but they do play 
# a big role in many data cleaning and preparation tasks. The stringr package 
# provide a cohesive set of functions designed to make working with strings 
# as easy as possible.
# stringr is built on top of stringi, which uses the ICU C library to provide 
# fast, correct implementations of common string manipulations. stringr 
# focusses on the most important and commonly used string manipulation 
# functions whereas stringi provides a comprehensive set covering almost 
# anything you can imagine. If you find that stringr is missing a function 
# that you need, try looking in stringi. Both packages share similar 
# conventions, so once you’ve mastered stringr, you should find stringi 
# similarly easy to use.

library(stringr)

# All functions in stringr start with str_ and take a vector of strings as 
# the first argument.
# Most string functions work with regular expressions, a concise language 
# for describing patterns of text.


# * Modify ----------------------------------------------------------------
str_c("hello", "world")
str_c("hello", "world", sep = " ")

str_dup("hello", 5)

str_flatten(c("hello", "world"))
str_flatten(c("hello", "world"), collapse = "_ _")

# Padding
str_pad("hello", width = 8, side = "left", pad = "x")
str_pad("hello", width = 8, side = "right", pad = "y")
str_pad("hello", width = 7, side = "both", pad = "z")
str_pad("hello", 10, pad = c("-", "_", " "))
str_pad("hellohello", width = 8, side = "left", pad = "x")

# Case
str_to_lower("HEllO")
str_to_upper("hello")
str_to_title("hello world")
str_to_sentence("hello world")

# Sorting
str_order(c("world", "hello"))
str_sort(c("world", "hello"))

# Splitting
str_split("hello", "e")
str_split("hello", "e") %>% unlist()

s <- "This string is moderately long"
# length(s)
nchar(s)
rbind(
  str_trunc(s, 20, "right"),
  str_trunc(s, 20, "left"),
  str_trunc(s, 20, "center")
)

# Trimming 
str_trim("hello")
str_trim("hello ")
str_trim("  hello   world  ")

str_squish("hello")
str_squish("hello ")
str_squish("  hello   world  ")


# * Count Patterns --------------------------------------------------------
str_length("hello")

str_count("hello", "h") # h letter
str_count("hello;", "\\w") # word + numbers
str_count("hello;", "[:punct:]") # punctuation
str_count("hello", "\\d") # digits


# * Detect Patterns -------------------------------------------------------
str_locate("hello", "e")
str_locate("hello", "ll")

str_detect("hello", "h")
str_detect("hello", "\\w")
str_detect("hello", "\\d")

str_which(c("hello", "world"), "ll")

str_starts("hello", "h")
str_ends("hello", "\\s") # space


# * Extract Patterns ------------------------------------------------------
str_sub("hello", 2, 3)
str_sub("hello world", 4, 8)

str_extract("hello", "el")
str_extract("hello", "lo")
str_extract(c("hello", "worldw"), "w")
str_extract_all(c("hello", "worldw"), "w")
str_extract_all(c("hello", "worldw"), "w") %>% unlist()


# * Removing & Replacing Patterns -----------------------------------------
str_remove("hello", "h")
str_remove(c("hello", "world"), "l")
str_remove_all(c("hello world", "hello world"), "l")

str_replace("hello", "h", "c")
str_replace(c("hello world", "hello world"), "l", "t")
str_replace_all(c("hello world", "hello world"), "l", "t")


# * Testing Patterns ------------------------------------------------------
str_view("hello world!", "world")
str_view("hello world!", "\\d")
str_view("hello world!", "\\w")
str_view("hello world!", "\\w*")
str_view("hello world!", "\\w*\\s\\w+")
str_view("hello world!", "\\w{3}")


# * Base R Functions ------------------------------------------------------

grep("e", "hello")
grep("e", c("hello", "world"))
grep("e", c("hello", "world"), value = TRUE)
grepl("e", c("hello", "world"))
gsub("e", "a", "hello")
nchar("hello")
paste("hello", "world")
tolower("HEllO")
toupper("hello")



# Forcats -----------------------------------------------------------------

# https://forcats.tidyverse.org/

# R uses factors to handle categorical variables, variables that have a fixed 
# and known set of possible values. Factors are also helpful for reordering 
# character vectors to improve display. The goal of the forcats package is to 
# provide a suite of tools that solve common problems with factors, including 
# changing the order of levels or the values.

library(dplyr)
library(ggplot2)
library(forcats)

str(gss_cat)


# * Count Levels ----------------------------------------------------------
levels(gss_cat$partyid)

fct_count(gss_cat$partyid)
gss_cat %>% count(partyid)


# * Order Levels ----------------------------------------------------------

levels(gss_cat$rincome)

# We can use the function fct_relevel() when we need to manually reorder 
# our factor levels. In addition to the factor, you give it a character 
# vector of level names, and specify where you want to move them. It defaults 
# to moving them to the front, but you can move them after another level 
# with the argument after. If you want to move it to the end, you set after 
# equal to Inf.

fct_relevel(gss_cat$rincome, c("Lt $1000", "$1000 to 2999")) %>%
  levels()

# It’s often useful to change the order of the factor levels in a visualisation.
# We can improve visualization by reordering the levels of relig using 
# fct_reorder().

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

# Reorder income by average age
levels(rincome_summary$rincome)
fct_reorder(rincome_summary$rincome, rincome_summary$age) %>% 
  levels()
# unordered chart, difficult to read
rincome_summary %>% 
  ggplot(aes(age, rincome)) + 
  geom_point()
# ordered chart, easy to read
rincome_summary %>% 
  mutate(rincome = fct_reorder(rincome, age)) %>%
  ggplot(aes(age, rincome)) + 
  geom_point()

# Finally, for bar plots, you can use fct_infreq() to order levels in 
# increasing frequency: this is the simplest type of reordering because 
# it doesn’t need any extra variables. You may want to combine with fct_rev().

fct_infreq(gss_cat$rincome) %>% 
  levels()

fct_infreq(gss_cat$rincome) %>% 
  fct_rev() %>% 
  levels()

gss_cat %>%
  mutate(rincome = rincome %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(rincome)) +
  geom_bar() + 
  coord_flip()


# * Modify Levels ---------------------------------------------------------

levels(gss_cat$partyid)

# More powerful than changing the orders of the levels is changing their 
# values. This allows you to clarify labels for publication, and collapse 
# levels for high-level displays. The most general and powerful tool is 
# fct_recode(). It allows you to recode, or change, the value of each level.

# fct_recode() will leave levels that aren’t explicitly mentioned as is, 
# and will warn you if you accidentally refer to a level that doesn’t exist.
# To combine groups, you can assign multiple old levels to the same new level.

fct_recode(
  gss_cat$partyid,
  "Republican, strong" = "Strong republican",
  "Republican, weak" = "Not str republican",
  "Independent, near rep" = "Ind,near rep",
  "Independent, near dem" = "Ind,near dem",
  "Democrat, weak" = "Not str democrat",
  "Democrat, strong" = "Strong democrat"
) %>% 
  fct_count()

fct_recode(
  gss_cat$partyid,
  "Republican, strong" = "Strong republican",
  "Republican, weak" = "Not str republican",
  "Independent, near rep" = "Ind,near rep",
  "Independent, near dem" = "Ind,near dem",
  "Democrat, weak" = "Not str democrat",
  "Democrat, strong" = "Strong democrat",
  "Other" = "No answer",
  "Other" = "Don't know",
  "Other" = "Other party"
) %>% 
  fct_count()

# If you want to collapse a lot of levels, fct_collapse() is a useful 
# variant of fct_recode(). For each new variable, you can provide a vector 
# of old levels

fct_collapse(
  gss_cat$partyid,
  other = c("No answer", "Don't know", "Other party"),
  rep = c("Strong republican", "Not str republican"),
  ind = c("Ind,near rep", "Independent", "Ind,near dem"),
  dem = c("Not str democrat", "Strong democrat")
) %>%
  fct_count()

# Sometimes you just want to lump together all the small groups to make 
# a plot or table simpler. That’s the job of fct_lump().
# The default behavior is to progressively lump together the smallest 
# groups, ensuring that the aggregate is still the smallest group.
# We can use the n parameter to specify how many groups (excluding other) 
# we want to keep.

fct_lump(gss_cat$partyid) %>% 
  fct_count()

fct_lump(gss_cat$partyid, n = 5) %>% 
  fct_count()

fct_lump(gss_cat$partyid, n = 3, other_level = "Extra") %>% 
  fct_count()



# Lubridate ---------------------------------------------------------------

# https://lubridate.tidyverse.org/

# Date-time data can be frustrating to work with in R. R commands 
# for date-times are generally unintuitive and change depending on the 
# type of date-time object being used. Moreover, the methods we use with 
# date-times must be robust to time zones, leap days, daylight savings times, 
# and other time related quirks, and R lacks these capabilities in some 
# situations. Lubridate makes it easier to do the things R does with date-times 
# and possible to do the things R does not.

library(lubridate)

# * Parse -----------------------------------------------------------------

# Date
d1 <- ymd("2022-01-01")
d2 <- ymd("2022-01-02")
class(d1)

# Datetime
t1 <- ymd_hms("2022-01-01 10:30:15")
t2 <- ymd_hms("2022-01-02 22:30:15")
class(t1)


# * Differences -----------------------------------------------------------

# Time Differences
d2 - d1
difftime(d2, d1, units = "auto")
difftime(d2, d1, units = "hours")
difftime(d2, d1, units = "weeks")
d1 - d2


# * Extract Time Periods --------------------------------------------------

second(t1)
minute(t1)
hour(t1)
day(t1)
week(t1)
month(t1)
month(t1, label = TRUE, abbr = TRUE)
month(t1, label = TRUE, abbr = FALSE)
quarter(t1)
semester(t1)
year(t1)

it_locale <- "it_IT.UTF-8"
en_locale <- "en_US.UTF-8"
Sys.getlocale()
Sys.setlocale("LC_ALL", locale = en_locale)
month(d1, label = TRUE, abbr = TRUE)
month(d1, label = TRUE, abbr = FALSE)
Sys.setlocale("LC_ALL", locale = it_locale)


# * Round -----------------------------------------------------------------

t2

round_date(t2, unit = "hour")
round_date(t2, unit = "month")

floor_date(t2, unit = "hour")
ceiling_date(t2, unit = "hour")



# Readr -------------------------------------------------------------------


# * Readr -----------------------------------------------------------------

# https://readr.tidyverse.org/

# The goal of readr is to provide a fast and friendly way to read rectangular 
# data (like csv, tsv, and fwf). It is designed to flexibly parse many types 
# of data found in the wild, while still cleanly failing when data unexpectedly 
# changes.

library(readr)
readr_example()
csv_example <- readr_example("mtcars.csv") 
read_csv(csv_example)


# * Readxl ----------------------------------------------------------------

# https://readxl.tidyverse.org/

# The readxl package makes it easy to get data out of Excel and into R. 
# Compared to many of the existing packages (e.g. gdata, xlsx, xlsReadWrite) 
# readxl has no external dependencies, so it’s easy to install and use on all 
# operating systems. It is designed to work with tabular data.
# readxl supports both the legacy .xls format and the modern xml-based .xlsx 
# format. The libxls C library is used to support .xls, which abstracts away 
# many of the complexities of the underlying binary format. To parse .xlsx, 
# we use the RapidXML C++ library.

install.packages("readxl")
library(readxl)

readxl_example()
xlsx_example <- readxl_example("datasets.xlsx") 
excel_sheets(xlsx_example)
read_excel(xlsx_example, sheet = "mtcars")


