

suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(dplyr); library(tidyr)
  library(lubridate); library(plotly); library(forecast); library(DT); library(scales)
})


# 1. DATA

if (!file.exists("clean_sales_data.csv"))
  stop("clean_sales_data.csv not found. Put it next to app.R (or run generate_kolkata_data.R).")

DAYS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
sales <- read.csv("clean_sales_data.csv", stringsAsFactors = FALSE) %>%
  mutate(Order_Date = as.Date(Order_Date),
         Weekday    = factor(Weekday, levels = DAYS)) %>%
  filter(Event_Type == "Payment")                      # refunds excluded

min_date <- min(sales$Order_Date); max_date <- max(sales$Order_Date)
ALL_DAYS <- seq(min_date, max_date, by = "day")
RS  <- "\u20b9"
inr <- function(x) paste0(RS, formatC(round(x), format = "d", big.mark = ","))

# warm "Kolkata" palette (saffron, turmeric, chilli, leaf, river-blue, jamun)
PAL <- c("#E8590C", "#F59F00", "#2F9E44", "#D6336C", "#1C7ED6", "#7048E8", "#0CA678", "#FF922B")

tab_intro <- function(what, why) {
  div(class = "intro-card",
      tags$div(tags$b("What you're looking at: "), what),
      tags$div(style = "margin-top:4px;", tags$b("Why it matters: "), why))
}


# 2. FORECASTING HELPERS (robust: never returns "not available")

daily_series <- function(df, col) {
  agg <- df %>% group_by(Order_Date) %>% summarise(v = sum(.data[[col]]), .groups = "drop")
  data.frame(Order_Date = ALL_DAYS) %>%
    left_join(agg, by = "Order_Date") %>%
    mutate(v = ifelse(is.na(v), 0, v))
}

# Tries the requested model, then falls back step by step so that
# even slow / sparse items always get a forecast.
fit_forecast <- function(y, h, choice = "Auto") {
  y   <- as.numeric(y); y[is.na(y)] <- 0
  tsy <- ts(y, frequency = 7)
  n   <- length(y); nz <- sum(y > 0)
  quiet <- function(expr) suppressWarnings(tryCatch(expr, error = function(e) NULL))

  cands <- list()
  if (nz >= 15 && n >= 28) {
    if (choice %in% c("Auto", "ETS"))   cands$ETS   <- quiet(ets(tsy, additive.only = TRUE))
    if (choice %in% c("Auto", "ARIMA")) cands$ARIMA <- quiet(auto.arima(tsy))
  }
  cands <- cands[!sapply(cands, is.null)]

  if (length(cands) > 0) {
    aics  <- sapply(cands, function(m) quiet(AIC(m)) %||% Inf)
    best  <- names(cands)[which.min(aics)]
    fc    <- quiet(forecast(cands[[best]], h = h, level = 95))
    if (!is.null(fc)) return(list(fc = fc, model = best, note = NULL))
  }
  # fallbacks for sparse / short series
  fc <- if (n >= 14) quiet(snaive(tsy, h = h, level = 95)) else NULL
  if (!is.null(fc) && nz >= 5)
    return(list(fc = fc, model = "Seasonal-naive",
                note = "Item sells on few days, so a simple weekly-pattern model is used."))
  list(fc = meanf(tsy, h = h, level = 95), model = "Average",
       note = "Very sparse item, so the historical average is used.")
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

metrics <- function(actual, pred) {
  data.frame(
    MAE   = mean(abs(actual - pred)),
    RMSE  = sqrt(mean((actual - pred)^2)),
    `sMAPE_%` = mean(200 * abs(actual - pred) / (abs(actual) + abs(pred) + 1e-9)),
    check.names = FALSE)
}

# 3. UI

custom_css <- "
.skin-blue .main-header .logo{background:#C2410C;font-weight:700;letter-spacing:.3px}
.skin-blue .main-header .logo:hover{background:#9A3412}
.skin-blue .main-header .navbar{background:linear-gradient(90deg,#E8590C,#F59F00)}
.skin-blue .main-sidebar{background:#7C2D12}
.skin-blue .sidebar-menu>li>a{color:#FFEDD5;font-size:15px}
.skin-blue .sidebar-menu>li.active>a,.skin-blue .sidebar-menu>li:hover>a{background:#9A3412;color:#fff;border-left-color:#FCC419}
.skin-blue .sidebar label,.skin-blue .sidebar .control-label{color:#FFEDD5}
.content-wrapper,.right-side{background:#FFF8EC}
.content-wrapper h3{color:#9A3412;font-weight:700;margin-bottom:12px}
.box{border-radius:10px;box-shadow:0 2px 8px rgba(124,45,18,.12);border-top:0}
.box.box-solid>.box-header{border-radius:10px 10px 0 0}
.box.box-solid.box-primary>.box-header{background:#E8590C}
.box.box-solid.box-primary{border:1px solid #E8590C}
.box.box-solid.box-success>.box-header{background:#2F9E44}
.box.box-solid.box-success{border:1px solid #2F9E44}
.box.box-solid.box-warning>.box-header{background:#F59F00;color:#fff}
.box.box-solid.box-warning{border:1px solid #F59F00}
.box.box-solid.box-danger>.box-header{background:#D6336C}
.box.box-solid.box-danger{border:1px solid #D6336C}
.box.box-solid.box-info>.box-header{background:#1C7ED6}
.box.box-solid.box-info{border:1px solid #1C7ED6}
.intro-card{background:#FFF3BF;border-left:5px solid #F59F00;padding:10px 14px;margin-bottom:14px;border-radius:6px;color:#5C3D00}
.small-box{border-radius:10px}
.btn-go{background:#E8590C;color:#fff;font-weight:600;border:0}
.btn-go:hover{background:#C2410C;color:#fff}
.model-note{color:#9A3412;font-weight:600;margin-top:6px}
"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Rongin Rasoi \u00b7 Kolkata"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Overview",        tabName = "overview", icon = icon("chart-line")),
      menuItem("2. Item Analysis",   tabName = "items",    icon = icon("boxes")),
      menuItem("3. Category & Time", tabName = "patterns", icon = icon("calendar")),
      menuItem("4. Forecast",        tabName = "forecast", icon = icon("bullseye")),
      menuItem("5. Reorder Advisor", tabName = "reorder",  icon = icon("exclamation-triangle"))
    ),
    hr(),
    div(style = "padding:0 12px;",
      dateRangeInput("dateFilter", "Date range (tabs 1-3)", start = min_date, end = max_date,
                     min = min_date, max = max_date),
      selectInput("catFilter", "Category (tabs 1-3)",
                  choices = c("All", sort(unique(sales$Category))), selected = "All"),
      helpText(style = "color:#FDBA74;", "Tabs 4 & 5 always use the full history for better forecasts.")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    tabItems(
      # ---------- TAB 1 ----------
      tabItem("overview",
        h3("1. Overview \u2014 restaurant health at a glance"),
        tab_intro("Revenue, units and daily average, a revenue trend, and a Pareto (80/20) chart of the dishes that earn the most.",
                  "A 10-second health check for the owner, and it shows which dishes must never run out."),
        fluidRow(valueBoxOutput("kpiRevenue", 3), valueBoxOutput("kpiUnits", 3),
                 valueBoxOutput("kpiAvg", 3),     valueBoxOutput("kpiTop", 3)),
        fluidRow(
          box(width = 7, title = "Revenue Trend", status = "primary", solidHeader = TRUE,
              radioButtons("trendGran", NULL, c("Daily", "Weekly", "Monthly"), "Weekly", inline = TRUE),
              plotlyOutput("revenueTrend", height = 320)),
          box(width = 5, title = "Pareto (ABC) Analysis", status = "warning", solidHeader = TRUE,
              helpText("Bars = revenue per dish; line = cumulative share. The dashed line marks 80%."),
              plotlyOutput("paretoPlot", height = 320)))
      ),
      # ---------- TAB 2 ----------
      tabItem("items",
        h3("2. Item Analysis \u2014 what to push, what to drop"),
        tab_intro("Top 10 best sellers, the slowest-moving dishes (by sales velocity) and a drill-down for any single dish.",
                  "Velocity = units per active day, so a dish that sold once in a big party order is not mistaken for a healthy item."),
        fluidRow(
          box(width = 6, title = "Top 10 Best Sellers (revenue)", status = "success", solidHeader = TRUE,
              plotlyOutput("topItems", height = 350)),
          box(width = 6, title = "Slow Movers (lowest velocity)", status = "danger", solidHeader = TRUE,
              DTOutput("slowItems"))),
        fluidRow(
          box(width = 12, title = "Dish Drill-down", status = "primary", solidHeader = TRUE,
              selectizeInput("itemPick", "Choose a dish", choices = sort(unique(sales$Product_Name))),
              plotlyOutput("itemTrend", height = 300)))
      ),
      # ---------- TAB 3 ----------
      tabItem("patterns",
        h3("3. Category & Time \u2014 what sells, and when"),
        tab_intro("Revenue share by category, how each category trends, and a weekday \u00d7 hour demand heatmap.",
                  "Shows when to roster extra kitchen staff (weekend dinners, Durga Puja week) and which categories carry the business."),
        fluidRow(
          box(width = 5, title = "Revenue Share by Category", status = "primary", solidHeader = TRUE,
              plotlyOutput("catShare", height = 330)),
          box(width = 7, title = "Category Trend (weekly)", status = "success", solidHeader = TRUE,
              plotlyOutput("catTrend", height = 330))),
        fluidRow(
          box(width = 12, title = "Weekday \u00d7 Hour Demand Heatmap", status = "warning", solidHeader = TRUE,
              helpText("Darker = more revenue. Look for the lunch (12-2pm) and dinner (7-10pm) peaks."),
              plotlyOutput("heatmap", height = 360)))
      ),
      # ---------- TAB 4 ----------
      tabItem("forecast",
        h3("4. Forecast \u2014 predicting demand ahead"),
        tab_intro("Pick the whole store or one dish. The app tests the model on the last 20% of real days, then forecasts forward with a 95% band.",
                  "Judges can see the model was validated on unseen data and compared with a simple 'same as last week' baseline."),
        fluidRow(
          box(width = 3, title = "Settings", status = "primary", solidHeader = TRUE,
              radioButtons("fcLevel", "Forecast level", c("Overall store", "Single item")),
              conditionalPanel("input.fcLevel == 'Single item'",
                selectizeInput("fcItem", "Dish", choices = sort(unique(sales$Product_Name)))),
              radioButtons("fcMetric", "Forecast what?", c("Revenue (INR)", "Units sold")),
              radioButtons("fcModel", "Model", c("Auto", "ARIMA", "ETS"), inline = TRUE),
              sliderInput("fcHorizon", "Days ahead", 7, 90, 30, step = 7),
              actionButton("runForecast", "Run forecast", icon = icon("play"), class = "btn-go")),
          box(width = 9, title = "History + Forecast", status = "success", solidHeader = TRUE,
              plotlyOutput("forecastPlot", height = 380),
              div(class = "model-note", textOutput("modelNote")))),
        fluidRow(
          box(width = 12, title = "Back-test: accuracy on the last 20% of real days", status = "info", solidHeader = TRUE,
              helpText("Lower is better. sMAPE is % error and stays valid on days with zero sales."),
              tableOutput("accTable")))
      ),
      # ---------- TAB 5 ----------
      tabItem("reorder",
        h3("5. Reorder Advisor \u2014 what to prep next week"),
        tab_intro("For the top dishes: 7-day demand forecast + safety stock (1.65 \u00d7 \u03c3 \u2248 95% service level) = suggested prep quantity.",
                  "This turns a forecast chart into a concrete kitchen / purchasing action."),
        fluidRow(
          box(width = 12, title = "Suggested Prep Quantities (next 7 days, units)", status = "danger", solidHeader = TRUE,
              selectInput("reorderN", "How many top dishes?", c(10, 15, 20, 38), selected = 15, width = "200px"),
              DTOutput("reorderTable")))
      )
    )
  )
)


# 4. SERVER

server <- function(input, output, session) {

  filtered <- reactive({
    df <- sales %>% filter(Order_Date >= input$dateFilter[1], Order_Date <= input$dateFilter[2])
    if (input$catFilter != "All") df <- df %>% filter(Category == input$catFilter)
    validate(need(nrow(df) > 0, "No sales in this selection."))
    df
  })

  # ----- KPIs -----
  output$kpiRevenue <- renderValueBox(valueBox(inr(sum(filtered()$Total_Sales)), "Total Revenue", icon("rupee-sign"), "orange"))
  output$kpiUnits   <- renderValueBox(valueBox(comma(sum(filtered()$Quantity_Sold)), "Units Sold", icon("shopping-basket"), "green"))
  output$kpiAvg     <- renderValueBox({
    df <- filtered(); valueBox(inr(sum(df$Total_Sales) / n_distinct(df$Order_Date)), "Avg Revenue / Day", icon("calculator"), "yellow")
  })
  output$kpiTop     <- renderValueBox({
    top <- filtered() %>% group_by(Product_Name) %>% summarise(r = sum(Total_Sales)) %>% slice_max(r, n = 1)
    valueBox(top$Product_Name[1], "Best-selling Dish", icon("star"), "red")
  })

  # ----- Revenue trend -----
  output$revenueTrend <- renderPlotly({
    df <- filtered()
    unit <- switch(input$trendGran, Daily = "day", Weekly = "week", Monthly = "month")
    agg <- df %>% mutate(Period = floor_date(Order_Date, unit, week_start = 1)) %>%
      group_by(Period) %>% summarise(Revenue = sum(Total_Sales), .groups = "drop")
    plot_ly(agg, x = ~Period, y = ~Revenue, type = "scatter", mode = "lines+markers",
            line = list(color = "#E8590C", width = 3), marker = list(color = "#F59F00", size = 6),
            fill = "tozeroy", fillcolor = "rgba(245,159,0,0.18)") %>%
      layout(yaxis = list(title = "Revenue (\u20b9)"), xaxis = list(title = ""))
  })

  # ----- Pareto -----
  output$paretoPlot <- renderPlotly({
    df <- filtered() %>% group_by(Product_Name) %>% summarise(Revenue = sum(Total_Sales), .groups = "drop") %>%
      arrange(desc(Revenue)) %>% mutate(CumPct = cumsum(Revenue) / sum(Revenue) * 100) %>%
      mutate(Product_Name = factor(Product_Name, levels = Product_Name))
    plot_ly(df) %>%
      add_bars(x = ~Product_Name, y = ~Revenue, name = "Revenue", marker = list(color = "#F59F00")) %>%
      add_lines(x = ~Product_Name, y = ~CumPct, name = "Cumulative %", yaxis = "y2",
                line = list(color = "#C2410C", width = 3)) %>%
      layout(xaxis = list(title = "", tickangle = -60, tickfont = list(size = 9)),
             yaxis = list(title = "Revenue (\u20b9)"),
             yaxis2 = list(overlaying = "y", side = "right", range = c(0, 100), title = "Cumulative %"),
             shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y2", y0 = 80, y1 = 80,
                                line = list(color = "#2F9E44", dash = "dash"))),
             legend = list(orientation = "h", y = 1.12), margin = list(b = 110, r = 50))
  })

  # ----- Top items -----
  output$topItems <- renderPlotly({
    df <- filtered() %>% group_by(Product_Name) %>% summarise(Revenue = sum(Total_Sales), .groups = "drop") %>%
      slice_max(Revenue, n = 10)
    plot_ly(df, x = ~Revenue, y = ~reorder(Product_Name, Revenue), type = "bar", orientation = "h",
            marker = list(color = "#2F9E44")) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Revenue (\u20b9)"), margin = list(l = 150))
  })

  # ----- Slow movers -----
  output$slowItems <- renderDT({
    df <- filtered() %>% group_by(Product_Name) %>%
      summarise(Total_Units = sum(Quantity_Sold), Days_Sold = n_distinct(Order_Date),
                Velocity = round(Total_Units / Days_Sold, 2), .groups = "drop") %>%
      arrange(Velocity) %>% head(15)
    datatable(df, rownames = FALSE, options = list(pageLength = 6, dom = "tp"))
  })

  # ----- Drill-down -----
  output$itemTrend <- renderPlotly({
    df <- filtered() %>% filter(Product_Name == input$itemPick) %>%
      group_by(Order_Date) %>% summarise(Units = sum(Quantity_Sold), .groups = "drop")
    validate(need(nrow(df) > 0, "This dish has no sales in the selected range."))
    plot_ly(df, x = ~Order_Date, y = ~Units, type = "bar", marker = list(color = "#D6336C")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Units sold per day"))
  })

  # ----- Category share / trend -----
  output$catShare <- renderPlotly({
    df <- filtered() %>% group_by(Category) %>% summarise(Revenue = sum(Total_Sales), .groups = "drop")
    plot_ly(df, labels = ~Category, values = ~Revenue, type = "pie", hole = 0.45,
            marker = list(colors = PAL), textinfo = "label+percent") %>%
      layout(showlegend = FALSE)
  })
  output$catTrend <- renderPlotly({
    df <- filtered() %>% group_by(Category, Period = floor_date(Order_Date, "week", week_start = 1)) %>%
      summarise(Revenue = sum(Total_Sales), .groups = "drop")
    plot_ly(df, x = ~Period, y = ~Revenue, color = ~Category, colors = PAL, type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Revenue (\u20b9)"))
  })

  # ----- Heatmap -----
  output$heatmap <- renderPlotly({
    df <- filtered() %>% group_by(Weekday, Order_Hour) %>% summarise(Revenue = sum(Total_Sales), .groups = "drop") %>%
      complete(Weekday = factor(DAYS, levels = DAYS), Order_Hour = 11:23, fill = list(Revenue = 0))
    plot_ly(df, x = ~Order_Hour, y = ~Weekday, z = ~Revenue, type = "heatmap",
            colors = colorRamp(c("#FFF8EC", "#FFD43B", "#F76707", "#9A3412"))) %>%
      layout(xaxis = list(title = "Hour of day (24h)", dtick = 1),
             yaxis = list(title = "", categoryorder = "array", categoryarray = rev(DAYS)))
  })

  # ----- Forecast (tab 4) -----
  fc_res <- eventReactive(input$runForecast, {
    col <- if (input$fcMetric == "Units sold") "Quantity_Sold" else "Total_Sales"
    df  <- if (input$fcLevel == "Single item") filter(sales, Product_Name == input$fcItem) else sales
    ser <- daily_series(df, col)
    n <- nrow(ser); test_len <- max(7, floor(n * 0.2))
    train <- ser$v[1:(n - test_len)]; test <- ser$v[(n - test_len + 1):n]

    bt   <- fit_forecast(train, test_len, input$fcModel)
    pred <- pmax(as.numeric(bt$fc$mean), 0)
    base <- rep(tail(train, 7), length.out = test_len)            # "same as last week"
    acc  <- rbind(cbind(Model = paste0("Our model (", bt$model, ")"), round(metrics(test, pred), 2)),
                  cbind(Model = "Baseline: same as last week",       round(metrics(test, base), 2)))

    final <- fit_forecast(ser$v, input$fcHorizon, input$fcModel)
    list(ser = ser, final = final, acc = acc, h = input$fcHorizon,
         label = if (col == "Total_Sales") "Revenue / day (\u20b9)" else "Units / day")
  }, ignoreNULL = FALSE)

  output$forecastPlot <- renderPlotly({
    r <- fc_res(); fc <- r$final$fc
    fdates <- max_date + seq_len(r$h)
    plot_ly() %>%
      add_lines(x = r$ser$Order_Date, y = r$ser$v, name = "Actual", line = list(color = "#1C7ED6", width = 2)) %>%
      add_ribbons(x = fdates, ymin = pmax(as.numeric(fc$lower[, 1]), 0), ymax = as.numeric(fc$upper[, 1]),
                  name = "95% range", fillcolor = "rgba(245,159,0,0.25)", line = list(width = 0)) %>%
      add_lines(x = fdates, y = pmax(as.numeric(fc$mean), 0), name = "Forecast",
                line = list(color = "#E8590C", width = 3)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = r$label), legend = list(orientation = "h", y = 1.1))
  })
  output$modelNote <- renderText({
    r <- fc_res()
    paste0("Model used: ", r$final$model, if (!is.null(r$final$note)) paste0(" \u2014 ", r$final$note) else "")
  })
  output$accTable <- renderTable(fc_res()$acc, striped = TRUE, bordered = TRUE)

  # ----- Reorder advisor (tab 5) -----
  output$reorderTable <- renderDT({
    n_top <- as.integer(input$reorderN)
    items <- sales %>% group_by(Product_Name) %>% summarise(r = sum(Total_Sales), .groups = "drop") %>%
      slice_max(r, n = n_top) %>% pull(Product_Name)
    out <- lapply(items, function(it) {
      ser <- daily_series(filter(sales, Product_Name == it), "Quantity_Sold")
      f   <- fit_forecast(ser$v, 7, "ETS")
      fq  <- sum(pmax(as.numeric(f$fc$mean), 0))
      sdv <- sd(tail(ser$v, 56))
      ss  <- 1.65 * sdv * sqrt(7)
      data.frame(Dish = it, Forecast_7d = round(fq), Safety_Stock = round(ss),
                 Suggested_Prep = ceiling(fq + ss), Model = f$model)
    })
    datatable(bind_rows(out), rownames = FALSE, options = list(pageLength = 15, dom = "tp"))
  })
}

shinyApp(ui, server)
