# R Coding ----

# Lecture 4: Tidyverse Functional Programming -----------------------------
# 2021/2022
# Marco Zanotti

# Goals:
# - Functional Programming
# - Purrr



# Tidyverse ---------------------------------------------------------------

# https://www.tidyverse.org/
# https://www.tidyverse.org/packages/
# https://tidyverse.tidyverse.org/index.html

# The tidyverse is an opinionated collection of R packages designed for 
# data science. All packages share an underlying design philosophy, 
# grammar, and data structures.

install.packages("tidyverse")
library(tidyverse)



# Purrr -------------------------------------------------------------------

# https://purrr.tidyverse.org/

# purrr enhances R’s functional programming (FP) toolkit by providing a 
# complete and consistent set of tools for working with functions and vectors.
# If you’ve never heard of FP before, the best place to start is the family 
# of map() functions which allow you to replace many for loops with code that 
# is both more succinct and easier to read.

library(purrr)


# * For Loop vs Functionals -----------------------------------------------

# For loops are not as important in R as they are in other languages because 
# R is a functional programming language. This means that it’s possible to
# wrap up for loops in a function, and call that function instead of using 
# the for loop directly.

# To see why this is important, consider this simple data frame:
  
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
df

# Imagine you want to compute the mean of every column. You could do that 
# with a for loop:
    
output <- vector("double", length(df))
output
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

# You realise that you’re going to want to compute the means of every column 
# pretty frequently, so you extract it out into a function:
    
col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  return(output)
}
col_mean(df)
  
# But then you think it’d also be helpful to be able to compute the median, 
# and the standard deviation, so you copy and paste your col_mean() function 
# and replace the mean() with median() and sd():
    
col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  return(output)
}
col_median(df)

col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  return(output)
}
col_sd(df)

# You’ve copied-and-pasted this code twice, so it’s time to think about how 
# to generalise it. Notice that most of this code is for-loop boilerplate and 
# it’s hard to see the one thing (mean(), median(), sd()) that is different 
# between the functions.
# We can generalise col_mean(), col_median() and col_sd() by adding an 
# argument that supplies the function to apply to each column:
  
col_summary <- function(df, fun) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- fun(df[[i]])
  }
  return(output)
}

col_summary(df, mean)
col_summary(df, median)
col_summary(df, sd)

# The idea of passing a function to another function is an extremely powerful 
# idea, and it’s one of the behaviours that makes R a functional programming 
# language. It might take you a while to wrap your head around the idea, 
# but it’s worth the investment. In the rest of the chapter, you’ll learn 
# about and use the purrr package, which provides functions that eliminate 
# the need for many common for loops. The apply family of functions in base 
# R (apply(), lapply(), tapply(), etc) solve a similar problem, but purrr 
# is more consistent and thus is easier to learn.

# The goal of using purrr functions instead of for loops is to allow you to 
# break common list manipulation challenges into independent pieces:
   
# How can you solve the problem for a single element of the list? Once 
# you’ve solved that problem, purrr takes care of generalising your solution 
# to every element in the list.
 
# If you’re solving a complex problem, how can you break it down into 
# bite-sized pieces that allow you to advance one small step towards a 
# solution? With purrr, you get lots of small pieces that you can compose 
# together with the pipe.
 
# This structure makes it easier to solve new problems. It also makes it 
# easier to understand your solutions to old problems when you re-read your 
# old code.


# * The Map Functions -----------------------------------------------------

# The pattern of looping over a vector, doing something to each element and 
# saving the results is so common that the purrr package provides a family 
# of functions to do it for you. There is one function for each type of output:
#   - map() makes a list
#   - map_lgl() makes a logical vector
#   - map_int() makes an integer vector
#   - map_dbl() makes a double vector
#   - map_chr() makes a character vector
# Each function takes a vector as input, applies a function to each piece, 
# and then returns a new vector that’s the same length (and has the same names) 
# as the input. The type of the vector is determined by the suffix to the 
# map function.

# Once you master these functions, you’ll find it takes much less time to 
# solve iteration problems. But you should never feel bad about using a for 
# loop instead of a map function. The map functions are a step up a tower of 
# abstraction, and it can take a long time to get your head around how they 
# work. The important thing is that you solve the problem that you’re working 
# on, not write the most concise and elegant code (although that’s definitely 
# something you want to strive towards!).
 
# Some people will tell you to avoid for loops because they are slow. 
# They’re wrong! (Well at least they’re rather out of date, as for loops 
# haven’t been slow for many years.) The chief benefits of using functions 
# like map() is not speed, but clarity: they make your code easier to write 
# and to read.
 
# We can use these functions to perform the same computations as the last 
# for loop. Those summary functions returned doubles, so we need to use 
# map_dbl():

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

# Compared to using a for loop, focus is on the operation being performed 
# (i.e. mean(), median(), sd()), not the bookkeeping required to loop over 
# every element and store the output. This is even more apparent if we use 
# the pipe:

# maps can be used in pipe chains easily
df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)

# There are a few differences between map_*() and our col_summary():
#   - all purrr functions are implemented in C. This makes them a little 
#     faster at the expense of readability
#   - the second argument, .f, the function to apply, can be a formula, 
#     a character vector, or an integer vector
#   - map_*() uses … ([dot dot dot]) to pass along additional arguments to 
#     .f each time it’s called
#   - map functions also preserve names
  
# used with lambda (on-the-fly) functions
map_dbl(df, ~ mean(., trim = .5))

# used ... to specify additional arguments of .f
map_dbl(df, mean, trim = .5)


# * The Power of Mapping --------------------------------------------------

# Imagine you want to fit a linear model to each group in a dataset. 
# The following toy example splits up the mpg dataset into seven pieces 
# (one for each value of class) and fits the same linear model to each piece.

# with base R lambda function syntax
models <- mpg %>% 
  split(.$class) %>% 
  map(function(df) lm(hwy ~ displ + cyl, data = df))
models

# The syntax for creating an anonymous function in R is quite verbose so 
# purrr provides a convenient shortcut: a one-sided formula.

# with purrr lambda function syntax
models <- mpg %>% 
  split(.$class) %>% 
  map(~ lm(hwy ~ displ + cyl, data = .))
models

# Here I’ve used . as a pronoun: it refers to the current list element 
# (in the same way that i referred to the current index in the for loop).

# When you’re looking at many models, you might want to extract a summary 
# statistic like the R^2. To do that we need to first run summary() and 
# then extract the component called r.squared. We could do that using the 
# shorthand for anonymous functions.
  
model_one_summary <- summary(models[[1]])
typeof(model_one_summary)
View(model_one_summary)
model_one_summary$r.squared
model_one_summary[["r.squared"]]

# extract summary for each model
models %>%
  map(summary)

# extract r.squared from each summary of each model
models %>%
  map(summary) %>%
  map_dbl( ~ .$r.squared)

# But extracting named components is a common operation, so purrr provides 
# an even shorter shortcut: you can use a string.

models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

# You can also use an integer to select elements by position.

models %>% 
  map(summary) %>% 
  map_dbl(8)


# * Mapping Over Multiple Arguments ---------------------------------------

# So far we’ve mapped along a single input. But often you have multiple 
# related inputs that you need iterate along in parallel. That’s the job of 
# the map2() and pmap() functions. 
# For example, imagine you want to simulate some random normals with 
# different means. You know how to do that with map().
  
mu <- list(5, 10, -3)
map(mu, rnorm, n = 5)

# What if you also want to vary the standard deviation? We could use 
# map2() which iterates over two vectors in parallel.

sigma <- list(1, 5, 10)  
map2(mu, sigma, rnorm, n = 5)

# You could also imagine map3(), map4(), map5(), map6() etc, but that 
# would get tedious quickly. Instead, purrr provides pmap() which takes 
# a list of arguments. 
# For instance, you might use that if you wanted to vary the mean, 
# standard deviation, and number of samples.
  
n <- list(1, 3, 5)
args <- list(n, mu, sigma)
pmap(args, rnorm)

# If you don’t name the list’s elements, pmap() will use positional 
# matching when calling the function. That’s a little fragile, and makes 
# the code harder to read, so it’s better to name the arguments.
  
args_named <- list(mean = mu, sd = sigma, n = n)
pmap(args_named, rnorm)

# Since the arguments are all the same length, it makes also sense to store 
# them in a data frame.
  
params <- tibble(
  mean = c(5, 10, -3), 
  sd = c(1, 5, 10), 
  n = c(1, 3, 5)
) 
pmap(params, rnorm)


# * Invoking Different Functions ------------------------------------------

# There’s one more step up in complexity - as well as varying the arguments 
# to the function you might also vary the function itself.
  
f <- c("runif", "rnorm", "rpois")
f
param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)
param

# To handle this case, you can use invoke_map():

invoke_map(f, param, n = 5)

# The first argument is a list of functions or character vector of function 
# names. The second argument is a list of lists giving the arguments that 
# vary for each function. The subsequent arguments are passed on to every 
# function.

