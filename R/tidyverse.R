
# Tidyverse ----

library(tidyverse)
data(iris)
View(iris)




# 5) stringr
# 6) forcats

# 7) lubridate
# 8) tidyr
# 9) dplyr
# 10) purrr
# 11) ggplot2





# tidyevaluation ----------------------------------------------------------

f <- function(x) {
  print(x)
}

f(x = string)

string <- "ciao"
f(string)



# pipe operator -----------------------------------------------------------

library(magrittr)

# %>% # CTRL + MAIUSC + M

str(iris)
iris %>% str()

iris %>%
  as_tibble() %>%
  dplyr::rename_all( ~ str_remove_all(.x, "\\.")) %>%
  dplyr::mutate(ID = 1:150) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(across(SepalLength:PetalWidth, mean)) %>%
  ggplot2::ggplot(ggplot2::aes(x = SepalLength, y = SepalWidth, col = Species)) +
  ggplot2::geom_point()

iris_new <- as_tibble(iris)
iris_new <- dplyr::rename_all(iris_new, ~ str_remove_all(.x, "\\."))
iris_new <- dplyr::mutate(iris_new, ID = 1:150)
iris_new <- dplyr::group_by(iris_new, Species)
iris_new <- dplyr::summarise(iris_new, across(SepalLength:PetalWidth, mean))
ggplot2::ggplot(iris_new, ggplot2::aes(x = SepalLength, y = SepalWidth, col = Species)) +
  ggplot2::geom_point()



# tibble ------------------------------------------------------------------

iris
str(iris)
class(iris)

iris_tbl <- tibble::as_tibble(iris)
iris_tbl
str(iris_tbl)
class(iris_tbl)



# tidyr -------------------------------------------------------------------

library(tidyr)

dim(iris)
table(iris$Species)

# * pivot_longer ----------------------------------------------------------
iris_longer <- pivot_longer(
  data = iris,
  cols = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
)
dim(iris_longer)

iris_longer <- pivot_longer(
  data = iris,
  cols = -Species # tidyevaluation
)

iris_longer <- pivot_longer(
  data = iris,
  cols = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) # tidyevaluation
)


iris_id <- iris
iris_id$ID <- 1:nrow(iris_id)

iris_id_longer <- pivot_longer(
  data = iris_id,
  cols = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
)

iris_id_longer <- pivot_longer(
  data = iris_id,
  cols = -c("ID", "Species")
)

iris_id_longer <- pivot_longer(
  data = iris_id,
  cols = -c("ID", "Species"),
  names_to = "Type",
  values_to = "XYZ"
)

# * pivot_wider -----------------------------------------------------------

iris_wider <- pivot_wider(
  data = iris_longer,
  id_cols = "Species"
)

iris_id_wider <- pivot_wider(
  data = iris_id_longer,
  id_cols = c("ID", "Species"),
  names_from = Type,
  values_from = "XYZ"
)

iris_id_wider <- pivot_wider(
  data = iris_id_longer,
  id_cols = c(ID, Species),
  names_from = Type,
  values_from = "XYZ"
)

pivot_wider(
  data = iris_id_longer,
  id_cols = "ID",
  names_from = Type,
  values_from = "XYZ"
)

# * others tidyr ----------------------------------------------------------

iris_longer_separated <- separate(
  data = iris_longer,
  col = "name",
  into = c("Type", "Measure"),
  sep = "\\."
)

iris_longer_united <- unite(
  data = iris_longer_separated,
  col = "Name",
  "Type", "Measure",
  sep = "_#_"
)

unite(
  data = iris_longer_separated,
  col = "Name",
  Type, Measure,
  sep = "_#_"
)

iris_tbl_na <- iris_tbl
iris_tbl_na[1, 1] <- NA
iris_tbl_na
drop_na(iris_tbl_na)
iris_tbl_na %>%
  drop_na()



# dplyr -------------------------------------------------------------------

library(dplyr)

# * filter ----------------------------------------------------------------

iris_tbl_setosa <- filter(iris_tbl, Species == "setosa")

iris_tbl %>%
  filter(Species != "setosa", Sepal.Length >= 6) # , = &

iris_tbl %>%
  filter(Species != "setosa" & Sepal.Length >= 6)

iris_tbl %>%
  filter(Species != "setosa" | Sepal.Length >= 6)

iris %>%
  as_tibble() %>%
  filter(Species != "setosa", Sepal.Length >= 6)

# * slice -----------------------------------------------------------------

iris_tbl %>%
  slice(1)

iris_tbl %>%
  slice(1:4)

iris_tbl %>%
  slice(-1)

# * arrange ---------------------------------------------------------------

iris_tbl %>%
  arrange(Species) # alphabetical order on character columns

iris_tbl %>%
  arrange(desc(Species))

iris_tbl %>%
  arrange(desc(Species), Sepal.Length)

# * select ----------------------------------------------------------------

iris_tbl %>%
  select(Species)

iris_tbl %>%
  select(Sepal.Width:Petal.Width)

iris_tbl %>%
  select(-Species)

# contains()
# matches()
# starts_with()
# ends_with()

iris_tbl %>%
  select(Species, contains("Len"))

iris_tbl %>%
  select(Species, matches("^P"))

iris_tbl %>%
  select(Species, starts_with("P"))

iris_tbl %>%
  select(ends_with("es"))

# re-order columns with select
iris_tbl %>%
  select(Species, Sepal.Length, Petal.Length, Sepal.Width, Petal.Width)

iris_tbl %>%
  select(Species, everything())

# * rename ----------------------------------------------------------------

iris_tbl %>%
  rename(Sp = Species)

iris_tbl %>%
  rename(Sp = Species, SL = Sepal.Length)

# * relocate --------------------------------------------------------------

iris_tbl %>%
  relocate(Species, .before = Petal.Length)

iris_tbl %>%
  relocate(Species, Petal.Width, .after = Petal.Length)

iris_tbl %>%
  relocate(Species, .before = everything())

# * mutate ----------------------------------------------------------------

iris_tbl %>%
  mutate(ID = NA)

iris_tbl %>%
  mutate(Sepal.Length = Sepal.Length / 100)

iris_tbl %>%
  mutate(ID = NA, Sepal.Length = Petal.Length / 100)

iris_tbl %>%
  mutate(
    ID = NA,
    Sepal.Length = Petal.Length / 100
  )

iris_tbl %>%
  transmute(Sepal.Length = Petal.Length / 100)

iris_tbl %>%
  mutate(
    Sepal.Length = as.character(Sepal.Length),
    Sepal.Width = as.character(Sepal.Width),
    Petal.Length = as.character(Petal.Length),
    Petal.Width = as.character(Petal.Width)
  )

iris_tbl %>%
  mutate(across(Sepal.Length:Petal.Width, as.character))

# * summarise -------------------------------------------------------------

iris_tbl %>%
  summarise(
    mean_sepal_len = mean(Sepal.Length),
    mean_sepal_wid = mean(Sepal.Width),
    sd_sepal_len = sd(Sepal.Length),
    sd_sepal_wid = sd(Sepal.Width)
  )

iris_tbl %>%
  summarise(mean(Sepal.Length))

iris_tbl %>%
  summarise(across(c(Sepal.Length, Sepal.Width), c(mean, sd, min, max, var, median)))

iris_tbl %>%
  summarise(across(Sepal.Length:Petal.Width, list(mean = mean, stdev = sd)))

iris_tbl %>%
  summarise(across(Sepal.Length:Petal.Width, list(mean = mean, sd = sd))) %>%
  tidyr::pivot_longer(cols = everything()) %>%
  tidyr::separate(name, into = c("name", "type", "measure"), sep = "(\\.)|(_)") %>%
  tidyr::pivot_wider(id_cols = c(name, type), names_from = measure, values_from = value)

# * group_by --------------------------------------------------------------

iris_tbl %>%
  group_by(Species)

iris_tbl %>%
  group_by(Species) %>%
  summarise(n = n(), m = mean(Sepal.Length), sd = sd(Sepal.Length)) %>%
  ungroup()

iris_tbl %>%
  group_by(Species) %>%
  summarise(across(Sepal.Length:Petal.Width, list(mean = mean, sd = sd))) %>%
  ungroup()

iris_tbl %>%
  group_by(Species) %>%
  mutate(new_measure = Sepal.Length - mean(Petal.Length)) %>%
  ungroup()

iris_tbl %>%
  group_by(Species) %>%
  slice(1)

# * count -----------------------------------------------------------------

iris_tbl %>%
  count(Species)

iris_tbl %>%
  group_by(Species) %>%
  summarise(n = n())

# * others dplyr ----------------------------------------------------------

# distinct
iris_tbl %>%
  distinct() # remove duplicate

iris_tbl %>%
  distinct(Species, .keep_all = TRUE)

# bind_cols / bind_rows
# rbind, cbind
rbind(iris_tbl, iris_tbl)
cbind(iris_tbl, iris_tbl) %>% View()

bind_rows(iris_tbl, iris_tbl)
bind_cols(iris_tbl, iris_tbl)

iris_tbl_list <- list(iris_tbl, iris_tbl)
rbind(iris_tbl_list)
bind_rows(iris_tbl_list)
do.call(rbind, iris_tbl_list)

# * joining operations ----------------------------------------------------

iris_tbl_new <- tibble(
  Sepal.Length = c(1, 2, 3),
  Sepal.Heigth = c(5, 6, 7),
  Species = c("setosa", "virginica", "ciaociao")
)

iris_tbl_new_id <- tibble(
  id = c("id_1", "id_2", "id_3"),
  Sepal.Length = c(1, 2, 3),
  Sepal.Heigth = c(5, 6, 7),
  Species = c("setosa", "virginica", "ciaociao")
)

iris_tbl_id <- iris_tbl %>%
  mutate(id = paste0("id_", 1:nrow(iris_tbl)))

# left_join
iris_tbl %>%
  left_join(iris_tbl_new, by = "Species")

iris_tbl_new_id %>%
  left_join(iris_tbl_id, by = "id")

iris_tbl_new_id %>%
  left_join(iris_tbl_id, by = c("id", "Species"))

# right_join
# inner_join
# anti_join
# full_join



# stringr -----------------------------------------------------------------

library(stringr)

iris_tbl$Species
names(iris_tbl)

str_c("a", "b")
str_c("a", "b", sep = " ")

str_count("ciao", "c") # c letter
str_count("ciao;", "\\w") # word + numbers
str_count("ciao", "\\d") # digits

str_detect("ciao", "c")
str_detect("ciao", "\\w")
str_detect("ciao", "\\d")

str_starts("ciao", "c")
str_ends("ciao", "\\s") # space

str_dup("ciao", 5)

str_length("ciao")

str_extract("ciao", "ia")
str_extract("ciao", "au")
str_extract(c("ciao ciao", "ludovica"), "i")
str_extract_all(c("ciao ciao", "ludovica"), "i")
str_extract_all(c("ciao ciao", "ludovica"), "i") %>% unlist()

str_flatten(c("ciao", "ciao"))
str_flatten(c("ciao", "ciao"), collapse = "_ _")

str_locate("ciao", "i")
str_locate("ciao", "ia")

str_order(c("marco", "ciao", "ludovica"))
c("marco", "ciao", "ludovica")[str_order(c("marco", "ciao", "ludovica"))] # ordine alfabetico
str_sort(c("marco", "ciao", "ludovica"))

str_pad("ciao", width = 8, side = "left", pad = "x")
str_pad("ciao", width = 8, side = "right", pad = "y")
str_pad("ciao", width = 7, side = "both", pad = "z")
str_pad("a", 10, pad = c("-", "_", " "))

str_remove("ciao", "i")
str_remove(c("ciao ciao", "ludovica"), "i")
str_remove_all(c("ciao ciao", "ludovica"), "i")

str_replace("ciao", "i", "u")
str_replace(c("ciao ciao", "ludovica"), "ia", "um")
str_replace_all(c("ciao ciao", "ludovica"), "ia", "um")

str_remove_all(names(iris_tbl), "\\.")
str_replace_all(names(iris_tbl), "\\.", "_")

str_split("ciao", "ia")
str_split(names(iris_tbl), "\\.")

str_squish("ciao")
str_squish("ciao ")
str_squish("  ciao   marco  ")

str_trim("ciao")
str_trim("ciao ")
str_trim("  ciao   marco  ")

str_sub("ciao", 2, 3)
str_sub("ciao ciao", 2, 6)

str_to_lower("CIAo")
str_to_upper("ciaO")
str_to_title("ciao marco")
str_to_sentence("ciao marco")

x <- "This string is moderately long"
rbind(
  str_trunc(x, 20, "right"),
  str_trunc(x, 20, "left"),
  str_trunc(x, 20, "center")
)

str_which(c("ciao", "marco"), "ar")
str_detect(c("ciao", "marco"), "ar")

str_view("ciao marco come stai?", "marco")
str_view("ciao marco come stai?", "\\d")
str_view("ciao marco come stai?", "\\w")
str_view("ciao marco come stai?", "\\w*")
str_view("ciao marco come stai?", "\\w*\\s\\w+")
str_view("ciao marco come stai?", "\\w{3}")


grep("i", "ciao")
grep("i", c("marco", "ciao"))
grep("i", c("marco", "ciao"), value = TRUE)

grepl("i", c("marco", "ciao"))

nchar("ciao")

paste("ciao", "ciao")

tolower("CIAo")
toupper("ciaO")



# lubridate ---------------------------------------------------------------

library(lubridate)

Sys.getlocale()

"2010-12-15"

"2010-12-15 02:01:35"

as.Date("2010.12.15", format = "%Y.%m.%d")

ymd("2010-12-15")
ymd("2010.12-15")
ymd("2010/12/15")
ymd("2010_12_15")
mdy("2010-12-15")
dmy("2010-12-15")
ymd(20101215)

ymd_hms("2010-12-15 02:01:35")

d1 <- ymd("2021-05-12")
d2 <- ymd("2021-05-10")
t1 <- ymd_hms("2021-05-12 09:02:30")

d2 - d1
d1 - d2
difftime(d1, d2, units = "auto")
difftime(d1, d2, units = "hours")
difftime(d1, d2, units = "weeks")

day(d1)
month(d1)
month(d1, label = TRUE, abbr = TRUE)
month(d1, label = TRUE, abbr = FALSE)
year(d1)
week(d1)
quarter(d1)
semester(d1)
hour(t1)
minute(t1)
second(t1)

Sys.getlocale()
Sys.setlocale("LC_ALL", locale = "en_US.UTF-8")

month(d1, label = TRUE, abbr = TRUE)



# purrr -------------------------------------------------------------------

library(purrr)
iris_tbl <- tibble::as_tibble(iris)

iris_tbl

colMeans(iris_tbl[, -5])
apply(iris_tbl[, -5], 2, mean)
lapply(iris_tbl, mean)

map(iris_tbl, .f = mean)
map_dbl(iris_tbl, .f = mean)
map_df(iris_tbl, .f = mean)
map_chr(iris_tbl, .f = mean)
map_lgl(iris_tbl, .f = is.numeric)


x <- 1:10
y <- 1:10
x
y
map2(x, y, mean) # sbagliato perche' mean non ha due input vettori
map2(x, y, ~ mean(x = .x, trim = .y))

mean_ewise <- function(x1, x2) {
  x <- c(x1, x2)
  res <- mean(x)
  return(res)
}

map2(x, y, mean_ewise)
map2_dbl(x, y, mean_ewise)

map2(x, y, mean_ewise) %>%
  unlist()

x <- c(NA, 1:10)
y <- c(1:10, NA)
map2(x, y, mean, na.rm = TRUE)

map2(x, y, mean_ewise)
map2(x, y, mean_ewise)
map2_dbl(x, y, mean_ewise)


mean_ewise_narm <- function(x1, x2, na.rm = FALSE) {
  x <- c(x1, x2)
  res <- mean(x, na.rm = na.rm)
  return(res)
}

mean_ewise_narm <- function(x1, x2, ...) {
  x <- c(x1, x2)
  res <- mean(x, ...)
  return(res)
}

pmap()

purrr::set_names(iris_tbl, c("a", "b", "c", "d", "e"))



# ggplot2 -----------------------------------------------------------------

library(ggplot2)

iris_tbl <- tibble::as_tibble(iris)

ggplot()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point()

ggplot(iris_tbl) +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, col = Species)) # local mapping

ggplot(iris_tbl) +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_line()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  geom_line()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point() +
  geom_line()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(aes(col = Species), col = "black") +
  geom_line(col = "red")

ggplot() +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, col = Species), iris_tbl[1:10,]) +
  geom_line(aes(x = Sepal.Length, y = Sepal.Width), iris_tbl[11:20,])

ggplot(iris_tbl) +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, col = Species), data = iris_tbl[1:10,]) +
  geom_line(aes(x = Sepal.Length, y = Sepal.Width), data = iris_tbl[11:20,])

ggplot(mapping = aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(data = iris_tbl[1:10,]) +
  geom_line(data = iris_tbl[11:20,])


ggplot(iris_tbl, aes(x = Sepal.Length, col = Species)) +
  geom_bar()

ggplot(iris_tbl, aes(y = Sepal.Length, col = Species)) +
  geom_bar()

ggplot(iris_tbl, aes(x = Sepal.Length, col = Species)) +
  geom_bar() +
  coord_flip()

ggplot(iris_tbl, aes(x = Sepal.Length, fill = Species)) +
  geom_bar(position = "stack")

ggplot(iris_tbl, aes(x = Sepal.Length, fill = Species)) +
  geom_bar(position = "dodge")

ggplot(iris_tbl, aes(x = Sepal.Length, fill = Species)) +
  geom_bar(position = "fill")

ggplot(iris_tbl, aes(x = Sepal.Length)) +
  geom_histogram(col = 2, fill = 3)

ggplot(iris_tbl, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(col = 2) +
  facet_wrap(~ Species)

ggplot(iris_tbl, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(col = 2) +
  facet_wrap(~ Species, nrow = 3, ncol = 1)

ggplot(iris_tbl, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(col = 2) +
  geom_density() +
  facet_wrap(~ Species, nrow = 3, ncol = 1)

ggplot(iris_tbl, aes(x = Sepal.Length)) +
  # geom_histogram(col = 2) +
  geom_density()

ggplot(iris_tbl, aes(x = Sepal.Length)) +
  geom_boxplot() +
  coord_flip()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_boxplot() +
  geom_point(col = "grey60", alpha = 0.3) +
  facet_wrap(~ Species)

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_boxplot() +
  geom_point(col = "grey60", alpha = 0.3) +
  facet_wrap(~ Species, scales = "free")

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width)) +
  # geom_point() +
  geom_jitter()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width, col = Petal.Length)) +
  geom_jitter()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width, col = Species, size = Species)) +
  geom_jitter()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width, col = Species, shape = Species)) +
  geom_jitter()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width, col = Species, shape = Species, size = Species)) +
  geom_jitter()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter() +
  geom_smooth()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter() +
  geom_smooth(method = "lm")

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(aes(col = Species)) +
  geom_smooth(aes(group = Species), method = "lm", se = FALSE)

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(aes(col = Species)) +
  geom_smooth(aes(col = Species), method = "lm", se = FALSE)

# Labs & Themes
ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(aes(col = Species)) +
  geom_smooth(aes(col = Species), method = "lm", se = FALSE) +
  theme_minimal()

ggplot(iris_tbl, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(aes(col = Species)) +
  geom_smooth(aes(col = Species), method = "lm", se = FALSE) +
  labs(
    title = "XXX", subtitle = "aaa", caption = "ciao",
    x = "bbb", y = "", col = ""
  ) +
  theme_dark()



library(dplyr)
means <- iris_tbl %>%
  group_by(Species) %>%
  summarise(values = mean(Petal.Length))

ggplot(data = iris) +
  geom_boxplot(aes(x = Species, y = Petal.Length, fill = Species), alpha = 0.7) +
  geom_hline(yintercept = means$values, linetype = 2) +
  scale_fill_manual(values = c("lightpink", "violet", "hotpink")) +
  theme_bw() +
  theme(legend.position = "none")


iris_tbl %>%
  group_by(Species) %>%
  summarise(values = mean(Petal.Length)) %>%
  pull(values)
