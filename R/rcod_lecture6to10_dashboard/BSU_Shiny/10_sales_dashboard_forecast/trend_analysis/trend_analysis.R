# DS4B 102-R: PREDICTIVE WEB APPLICATIONS FOR BUSINESS ----
# TREND ANALYSIS ----

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

source("00_scripts/04_demand_forecast.R")


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

# 3.0 TREND EVALUATION ----

processed_data_tbl %>%
    aggregate_time_series("month") %>%
    generate_forecast(n_future = 24) %>%
    plot_forecast()


# 3.1 XGBOOST ----

source("10_sales_dashboard_forecast/model_forecast_xgb.R")

processed_data_tbl %>%
    aggregate_time_series("week") %>%
    generate_forecast_xgb(n_future = 200, seed = 123, 
                          mtry = 30, 
                          trees = 500, 
                          min_n = 5, 
                          tree_depth = 6, 
                          learn_rate = 0.01, 
                          loss_reduction = 0.01) %>%
    plot_forecast()


# 3.2 GLMNET ----

source("10_sales_dashboard_forecast/model_forecast_glmnet.R")

processed_data_tbl %>%
    aggregate_time_series("week") %>%
    generate_forecast_glmnet(n_future = 200, seed = 123, 
                             penalty = 1, mixture = 0.5) %>%
    plot_forecast()
