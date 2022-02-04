generate_new_bike <-
function(bike_model, category_1, category_2, frame_material, .ml_model) {
    
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
format_table <-
function(new_bike_tbl) {
    
    new_bike_tbl %>%
        mutate(price = scales::dollar(price, accuracy = 1)) %>%
        gather(key = "New Model Attribute", value = "value", -model, factor_key = TRUE) %>%
        spread(key = model, value = value)
    
}
bind_bike_prediction <-
function(bikes_tbl, new_bike_tbl) {
    
    bikes_tbl %>%
        separate_bike_description() %>%
        mutate(estimate = "Actual") %>%
        bind_rows(
            new_bike_tbl %>% mutate(estimate = "Prediction")
        ) %>%
        select(estimate, model, category_1, category_2, frame_material, price)
}
plot_bike_prediction <-
function(data, interactive = TRUE) {
    
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
