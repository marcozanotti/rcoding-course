# DS4B 102-R: SHINY APPS - LEVEL 1 ----
# R PACKAGES

r_pkgs <- c(
    # Core
    "tidyverse",
    "tidyquant",
    
    # Database
    "odbc",
    "RSQLite",
    
    # Visualization
    "plotly",
    
    # Shiny-verse
    "flexdashboard",
    "shiny",
    "shinyWidgets",
    "shinyjs",
    
    # Modeling & Machine Learning
    "parsnip",
    "rsample",
    "xgboost"
    )

install.packages(r_pkgs)