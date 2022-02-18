# Helper Functions


# Function to check packages already loaded into NAMESPACE
check_namespace <- function(pkgs) {

  pkgs_notloaded <- pkgs[!pkgs %in% loadedNamespaces()]
  if (length(pkgs_notloaded) == 0) {
    res <- NULL
  } else {
    res <- pkgs_notloaded
  }
  return(res)

}


# Function to install and load the specified packages
install_and_load <- function(pkgs, repos = getOption("repos")) {

  pkgs_inst <- pkgs[!pkgs %in% installed.packages()]

  if (length(pkgs_inst) == 0) {
    lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
    check_res <- check_namespace(pkgs)
    if (is.null(check_res)) {
      res <- "All packages correctly installed and loaded."
    } else {
      res <- paste0(
        "Problems loading packages ",
        paste0(check_res, collapse = ", "),
        "."
      )
    }

  } else {

    inst_res <- vector("character", length(pkgs_inst))

    for (i in seq_along(pkgs_inst)) {
      inst_res_tmp <- tryCatch(
        utils::install.packages(pkgs_inst[i], dependencies = TRUE, repos = repos, quiet = TRUE),
        error = function(e) e,
        warning = function(w) w
      )
      if (!is.null(inst_res_tmp)) {
        inst_res[i] <- inst_res_tmp$message
      }
    }

    pkgs_err <- pkgs_inst[!inst_res == ""]
    if (length(pkgs_err) == 0) {
      lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
      check_res <- check_namespace(pkgs)
      if (is.null(check_res)) {
        res <- "All packages correctly installed and loaded."
      } else {
        res <- paste0(
          "Problems loading packages ",
          paste0(check_res, collapse = ", "),
          "."
        )
      }
    } else {
      pkgs_noerr <- pkgs[!pkgs %in% pkgs_err]
      lapply(pkgs_noerr, library, character.only = TRUE, quietly = TRUE)
      check_res <- check_namespace(pkgs_noerr)
      if (is.null(check_res)) {
        res <- paste0(
          "Problems installing packages ",
          paste0(pkgs_err, collapse = ", "),
          "."
        )
      } else {
        res <- c(
          paste0(
            "Problems installing packages ",
            paste0(pkgs_err, collapse = ", "),
            "."
          ),
          paste0(
            "Problems loading packages ",
            paste0(check_res, collapse = ", "),
            "."
          )
        )
      }
    }

  }

  message(toupper(
    paste0(
      "\n\n\n",
      "\n==================================================================",
      "\nResults:\n ",
      res,
      "\n=================================================================="
    )
  ))
  return(invisible(res))

}


# Functions for rcod_lecture7_salesdash08_forecast
# function to perform time series aggregation
aggregate_time_series <- function(data, time_unit = "month") {

  output_tbl <- data %>%
    mutate(date = floor_date(order.date, unit = time_unit)) %>%
    group_by(date) %>%
    summarize(total_sales = sum(extended_price)) %>%
    ungroup() %>%
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))

  return(output_tbl)

}

# function to plot the time series
plot_time_series <- function(data) {

  g <- data %>%
    ggplot(aes(date, total_sales)) +
    geom_line(color = "#2c3e50") +
    geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
    geom_smooth(method = "loess", span = 0.2, size = 1) +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "")

  ggplotly(g, tooltip = "text")

}

# function to generate the forecast with ML models (Elastic Net or XGBoost)
generate_forecast <- function(data, n_future = 12, seed = NULL) {

  # frequency
  time_scale <- data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)
  
  # recipe
  ml_rcp <- recipe(total_sales ~ ., data = data %>% select(-label_text)) %>% 
    step_timeseries_signature(date) %>% 
    step_normalize(date_index.num) %>% 
    step_rm(matches("(iso)|(xts)|(lbl)|(hour)|(minute)|(second)|(hour12)|(am.pm)")) %>% 
    step_rm(date)
  
  # future data
  future_tbl <- data %>% 
    future_frame(.date_var = date, .length_out = n_future)
    
  if (time_scale %in% c("quarter", "year")) {
    
    model <- linear_reg(
      mode = "regression",
      penalty = 0.05,
      mixture = 0.5
    ) %>%
      set_engine(engine = "glmnet")
    
  } else {
    
    model <- boost_tree(
      mode = "regression",
      mtry = 20,
      trees = 500,
      min_n = 3,
      tree_depth = 8,
      learn_rate = 0.01,
      loss_reduction = 0.01
    ) %>%
      set_engine(engine = "xgboost")
    
  }

  set.seed(seed)
  wkfl_fit <- workflow() %>% 
    add_recipe(ml_rcp) %>% 
    add_model(model) %>% 
    fit(data = data %>% select(-label_text))
  
  prediction_tbl <- wkfl_fit %>% 
    predict(new_data = future_tbl) %>%
    bind_cols(future_tbl) %>%
    rename(total_sales = .pred) %>%
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>%
    add_column(key = "Prediction")

  output_tbl <- data %>%
    add_column(key = "Actual") %>%
    relocate(total_sales, .before = date) %>% 
    bind_rows(prediction_tbl)

  return(output_tbl)

}

# function to plot the forecast results
plot_forecast <- function(data) {

  time_scale <- data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)

  # Only 1 Prediction - points
  n_predictions <- data %>%
    filter(key == "Prediction") %>%
    nrow()

  g <- data %>%
    ggplot(aes(date, total_sales, color = key)) +
    geom_line() +
    scale_y_continuous(labels = scales::dollar_format()) +
    expand_limits(y = 0) +
    labs(x = "", y = "")

  # Yearly - LM Smoother
  if (time_scale %in% c("quarter", "year")) {
    g <- g +
      geom_smooth(method = "lm", size = 0.5)
  } else {
    g <- g + geom_smooth(method = "loess", span = 0.2, size = 0.5)
  }

  # Only 1 Prediction
  if (n_predictions == 1) {
    g <- g + geom_point(aes(text = label_text), size = 1)
  } else {
    g <- g + geom_point(aes(text = label_text), size = 0.01)
  }

  ggplotly(g, tooltip = "text")

}
