## Run this ONCE before anything else:  source("install_packages.R")
pkgs <- c("shiny", "shinydashboard", "dplyr", "tidyr", "lubridate",
          "plotly", "forecast", "DT", "scales", "tibble")
missing <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(missing)) install.packages(missing)
cat("All packages installed.\n")
