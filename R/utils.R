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

generate_forecast <- function(data, n_future = 12, seed = NULL) {

  train_tbl <- data %>%
    tk_augment_timeseries_signature()

  future_data_tbl <- data %>%
    tk_index() %>%
    tk_make_future_timeseries(
      n_future = n_future,
      inspect_weekdays = TRUE,
      inspect_months = TRUE
    ) %>%
    tk_get_timeseries_signature()

  time_scale <- data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)

  if (time_scale == "year") {
    model <- linear_reg(mode = "regression") %>%
      set_engine(engine = "lm") %>%
      fit.model_spec(
        total_sales ~ .,
        data = train_tbl %>% select(total_sales, index.num)
      )
  } else {
    set.seed(seed)
    model <- boost_tree(
      mode = "regression",
      mtry = 20,
      trees = 500,
      min_n = 3,
      tree_depth = 8,
      learn_rate = 0.01,
      loss_reduction = 0.01
    ) %>%
      set_engine(engine = "xgboost") %>%
      fit.model_spec(
        total_sales ~ .,
        data = train_tbl %>% select(-date, -label_text, -diff)
      )
  }

  prediction_tbl <- predict(model, new_data = future_data_tbl) %>%
    bind_cols(future_data_tbl) %>%
    select(.pred, index) %>%
    rename(total_sales = .pred, date = index) %>%
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>%
    add_column(key = "Prediction")

  output_tbl <- data %>%
    add_column(key = "Actual") %>%
    bind_rows(prediction_tbl)

  return(output_tbl)

}

plot_forecast <- function(data) {

  # Yearly - LM Smoother
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
  if (time_scale == "year") {
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
