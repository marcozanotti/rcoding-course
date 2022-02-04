# DS4B 102-R: PREDICTIVE WEB APPLICATIONS FOR BUSINESS ----
# DEMAND FORECAST ANALYSIS ----

# 1.0 LIBRARIES -----

# Core
library(tidyverse)
library(tidyquant)

# Interactive Visualizations
library(plotly)

# Modeling Libraries
library(parsnip)
library(timetk)

# Database
library(odbc)
library(RSQLite)


# 2.0 PROCESSED DATA ----
con <- dbConnect(RSQLite::SQLite(), "00_data/bikes_database.db")

bikes_tbl <- tbl(con, "bikes")
bikeshops_tbl <- tbl(con, "bikeshops")
orderlines_tbl <- tbl(con, "orderlines")

processed_data_tbl <- orderlines_tbl %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id")) %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    mutate(extended_price = quantity * price) %>%
    collect()

processed_data_tbl <- processed_data_tbl %>%    
    mutate(order.date = ymd(order.date)) %>%
    separate(location, into = c("city", "state"), sep = ", ") %>%
    
    separate(description, 
             into = c("category_1", "category_2", "frame_material"),
             sep = " - ") %>%
    
    select(order.date, order.id, order.line, state, quantity, price,
           extended_price, category_1:frame_material, bikeshop.name)

dbDisconnect(con)


# 3.0 TIME SERIES AGGREGATION ----

# 3.1 DATA MANIPULATION ----
time_unit <- "quarter"

time_plot_tbl <- processed_data_tbl %>%
    
    mutate(date = floor_date(order.date, unit = time_unit)) %>%
    
    group_by(date) %>%
    summarize(total_sales = sum(extended_price)) %>%
    ungroup() %>%
    
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))

time_plot_tbl

# 3.2 FUNCTION ----

# TODO - aggregate_time_series() 



# 3.3 TIME SERIES PLOT ----

data <- processed_data_tbl %>%
    aggregate_time_series("month")

g <- data %>%
    
    ggplot(aes(date, total_sales)) +
    
    geom_line(color = "#2c3e50") +
    geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
    geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "")


ggplotly(g, tooltip = "text")


# 3.4 FUNCTION ----

# TODO - MAKE FUNCTION 


# 4.0 FORECAST -----

# 4.1 SETUP TRAINING DATA AND FUTURE DATA ----

# TODO - timetk

# 4.2 MACHINE LEARNING ----

# TODO - XGBoost

# 4.3 MAKE PREDICTION & FORMAT OUTPUT ----

# TODO - predict

# 4.4 FUNCTION ----

# TODO - generate_forecast()



# 5.0 PLOT FORECAST ----

# 5.1 PLOT ----

# TODO - plot

# 5.2 FUNCTION ----

# TODO - plot_forecast()


# 6.0 SAVE FUNCTIONS ----

dump(c("aggregate_time_series", "plot_time_series", "generate_forecast", "plot_forecast"), 
     file = "00_scripts/03_demand_forecast.R")
