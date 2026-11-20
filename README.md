# Rongin Rasoi — Kolkata Restaurant Sales Forecast Dashboard (R Shiny)

## Files
| File | Purpose |
|---|---|
| `install_packages.R` | Installs required R packages (run once) |
| `generate_kolkata_data.R` | Builds the synthetic Kolkata POS dataset (optional) |
| `clean_sales_data.csv` | Ready-made dataset (~17k line items, Jun-Dec 2023, INR) |
| `app.R` | The 5-tab Shiny dashboard |

## Run (3 steps)
1. Install R (https://cran.r-project.org) and optionally RStudio.
2. In R / RStudio:
```r
setwd("path/to/rongin_rasoi")
source("install_packages.R")     # once
shiny::runApp("app.R")           # opens in your browser
```
(Optional) `source("generate_kolkata_data.R")` regenerates the dataset.

## Tabs
1. Overview — KPIs, revenue trend, Pareto 80/20
2. Item Analysis — top 10, slow movers (velocity), dish drill-down
3. Category & Time — category share/trend, weekday x hour heatmap
4. Forecast — store or dish, ARIMA/ETS/Auto, back-test vs "same as last week" baseline
5. Reorder Advisor — 7-day forecast + safety stock = prep quantity

## Notes for judges
- Data is synthetic, modelled on Kolkata patterns (weekend lift, Durga Puja/Kali Puja spikes, monsoon dip, 5% GST, UPI/Swiggy/Zomato). Single location by design.
- Item forecasts always return a result: ETS/ARIMA first, then seasonal-naive, then average for very sparse dishes.
