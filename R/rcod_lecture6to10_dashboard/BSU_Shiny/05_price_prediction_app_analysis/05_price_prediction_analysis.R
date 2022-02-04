# DS4B 102-R: PRODUCT PRICE PREDICTION APP ----
# XGBOOST REGRESSION MODEL ----

# GOAL: BUILD PREDICTION MODEL FOR PRICING ALGORITHM


# 1.0 LIBRARIES & DATA ----

# Standard
library(tidyverse)
library(tidyquant)
library(plotly)

# Modeling
library(rsample)
library(parsnip)

# Database
library(odbc)
library(RSQLite)


# Read Data
con <- dbConnect(RSQLite::SQLite(), "00_data/bikes_database.db")
bikes_tbl <- tbl(con, "bikes") %>% collect()
dbDisconnect(con)

# 2.0 PREPROCESS DATA ----

bikes_tbl %>%
    
    # 2.1 Separate Description Column ----
    separate(description, 
             sep    = " - ", 
             into   = c("category_1", "category_2", "frame_material"), 
             remove = FALSE) %>%
    
    # 2.2 Process Model Column ----
    # Fix typo
    mutate(model = case_when(
        model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
        model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
        model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
        TRUE ~ model
    )) %>%
    
    # separate using spaces
    separate(col     = model, 
             into    = str_c("model_", 1:7), 
             sep     = " ", 
             remove  = FALSE, 
             fill    = "right") %>%
    
    # creating a "base" feature
    mutate(model_base = case_when(
        
        # Fix Supersix Evo
        str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Fat CAAD bikes
        str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Beast of the East
        str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
        
        # Fix Bad Habit
        str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Scalpel 29
        str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
        
        # catch all
        TRUE ~ model_1)
    ) %>%
    
    # Get "tier" feature
    mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
    
    # Remove unnecessary columns
    select(-matches("model_[0-9]")) %>%
    
    # Create Flags
    mutate(
        black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
        hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
        team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
        red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
        ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
        dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
        disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
    )


# 3.0 XGBOOST MODEL -----

# 3.1 Create Model ----

# 3.2 Test Model ----

# 3.3 Save Model ----




# 4.0 MODULARIZE PREPROCESSING CODE ----

# 4.1 separate_bike_description() ----

# 4.2 separate_bike_model() ----

# 4.3 Test Functions ----

# 4.4 Save Functions ----

dump(c("separate_bike_model", "separate_bike_description"), 
     file = "00_scripts/01_process_data.R")


# 5.0 USER INPUT & PREDICTION ----

# 5.1 Inputs ----
bike_model <- "Jekyll Aluminum 1 Black"
category_1 <- "Mountain"
category_2 <- "Over Mountain"
frame_material <- "Aluminum"

# 5.2 Make Prediction ----


# 6.0 MODULARIZE NEW BIKE PREDICTION ----

# 6.1 generate_new_bike() Function ----  



# 6.2 Test ----

new_bike_tbl <- generate_new_bike(
    bike_model = "Jekyll Aluminum Black 1",
    category_1 = "Mountain",
    category_2 = "Over Mountain",
    frame_material = "Aluminum",
    .ml_model = model_xgboost
) 

new_bike_tbl



# 7.0 OUTPUT TABLE ----



# 8.0 OUTPUT PLOT PRODUCTS ----

# 8.1 bind_bike_predictions() function ----


# 8.2 plot_bike_prediction() function ----


# 8.3 Save functions ----

dump(c("generate_new_bike", "format_table", "bind_bike_prediction", "plot_bike_prediction"), 
     file = "00_scripts/02_make_predictions.R")
    