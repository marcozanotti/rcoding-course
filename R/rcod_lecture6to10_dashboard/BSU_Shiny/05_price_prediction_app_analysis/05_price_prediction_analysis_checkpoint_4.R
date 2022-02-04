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

train_tbl <- bikes_tbl %>%
    
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

train_tbl <- train_tbl %>%
    select(-c(bike.id, model, description, model_tier)) %>%
    select(price, everything())

train_tbl

set.seed(1234)
model_xgboost <- boost_tree(mode = "regression", 
           mtry = 30,
           learn_rate = 0.25,
           tree_depth = 7) %>%
    set_engine(engine = "xgboost") %>%
    fit(price ~ ., data = train_tbl)

# 3.2 Test Model ----

model_xgboost %>%
    predict(new_data = train_tbl %>% select(-price))

# 3.3 Save Model ----

write_rds(model_xgboost, path = "00_models/model_xgboost.rds")

read_rds("00_models/model_xgboost.rds")


# 4.0 MODULARIZE PREPROCESSING CODE ----

# 4.1 separate_bike_description() ----

data <- bikes_tbl

separate_bike_description <- function(data, keep_description_column = TRUE, append = TRUE) {
    
    if (!append) {
        data <- data %>% select(description)
    }
    
    output_tbl <- data %>% separate(description, 
             sep    = " - ", 
             into   = c("category_1", "category_2", "frame_material"), 
             remove = FALSE)
    
    if (!keep_description_column) output_tbl <- output_tbl %>% select(-description)
    
    return(output_tbl)
    
}

bikes_tbl %>% separate_bike_description(keep_description_column = FALSE, append = FALSE)

# 4.2 separate_bike_model() ----

data <- bikes_tbl

separate_bike_model <- function(data, keep_model_column = TRUE, append = TRUE) {
    
    if (!append) {
        data <- data %>% select(model)
    }
    
    # Fix typo
    output_tbl <- data %>% 
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
    
    if (!keep_model_column) output_tbl <- output_tbl %>% select(-model)
    
    return(output_tbl)
    
}

bikes_tbl %>% separate_bike_model()

# 4.3 Test Functions ----
bikes_tbl %>%
    separate_bike_description(keep_description_column = FALSE) %>%
    separate_bike_model(keep_model_column = FALSE)

# 4.4 Save Functions ----

dump(c("separate_bike_model", "separate_bike_description"), 
     file = "00_scripts/02_process_data.R")


# 5.0 USER INPUT & PREDICTION ----

# 5.1 Inputs ----
bike_model <- "Jekyll Aluminum 1"
category_1 <- "Mountain"
category_2 <- "Over Mountain"
frame_material <- "Aluminum"

# 5.2 Make Prediction ----
train_tbl

new_bike_tbl <- tibble(
    model = bike_model,
    category_1 = category_1,
    category_2 = category_2,
    frame_material = frame_material
) %>%
    separate_bike_model()

new_bike_tbl %>%
    predict(model_xgboost, new_data = .)


# 6.0 MODULARIZE NEW BIKE PREDICTION ----

# 6.1 generate_new_bike() Function ----  

bikes_tbl

generate_new_bike <- function(bike_model, category_1, category_2, frame_material, .ml_model) {
    
    new_bike_tbl <- tibble(
        model = bike_model,
        category_1 = category_1,
        category_2 = category_2,
        frame_material = frame_material
    ) %>%
        separate_bike_model()
    
    predict(.ml_model, new_data = new_bike_tbl) %>%
        bind_cols(new_bike_tbl) %>%
        rename(price = .pred)
    
}

new_bike_tbl <- generate_new_bike(
    bike_model = "Jekyll Aluminum Black Inc.",
    category_1 = "Mountain",
    category_2 = "Over Mountain",
    frame_material = "Aluminum",
    .ml_model = model_xgboost
)

bikes_tbl %>%
    separate_bike_description() %>%
    separate_bike_model() %>%
    bind_rows(new_bike_tbl) %>%
    tail()

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

format_table <- function(new_bike_tbl) {
    
    new_bike_tbl %>%
        mutate(price = scales::dollar(price, accuracy = 1)) %>%
        gather(key = "New Model Attribute", value = "value", -model, factor_key = TRUE) %>%
        spread(key = model, value = value)
    
}

new_bike_tbl %>% format_table()


# 8.0 OUTPUT PLOT PRODUCTS ----

# 8.1 bind_bike_predictions() function ----

bind_bike_prediction <- function(bikes_tbl, new_bike_tbl) {
    
    bikes_tbl %>%
        separate_bike_description() %>%
        mutate(estimate = "Actual") %>%
        bind_rows(
            new_bike_tbl %>% mutate(estimate = "Prediction")
        ) %>%
        select(estimate, model, category_1, category_2, frame_material, price)
}

bind_bike_prediction(bikes_tbl, new_bike_tbl) %>% tail()


# 8.2 plot_bike_prediction() function ----

plot_bike_prediction <- function(data, interactive = TRUE) {
    
    g <- data %>%
        mutate(category_2 = fct_reorder(category_2, price)) %>%
        mutate(label_text = str_glue("Unit Price: {scales::dollar(price, accuracy = 1)}
                                 Model: {model}
                                 Bike Type: {category_1}
                                 Bike Family: {category_2}
                                 Frame Material: {frame_material}")) %>%
        
        ggplot(aes(category_2, price, color = estimate)) +
        
        geom_violin() +
        geom_jitter(aes(text = label_text), width = 0.1, alpha = 0.5) +
        facet_wrap(~ frame_material) +
        
        coord_flip() +
        
        scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
        scale_color_tq() +
        theme_tq() +
        theme(strip.text.x = element_text(margin = margin(5, 5, 5, 5))) +
        labs(title = "", x = "", y = "Log Scale")
    
    
    if (interactive) {
        return(ggplotly(g, tooltip = "text"))
    } else {
        return(g)
    }
    
}

bind_bike_prediction(bikes_tbl, new_bike_tbl) %>%
    plot_bike_prediction(interactive = TRUE)


# 8.3 Save functions ----

dump(c("generate_new_bike", "format_table", "bind_bike_prediction", "plot_bike_prediction"), 
     file = "00_scripts/03_make_predictions.R")
    