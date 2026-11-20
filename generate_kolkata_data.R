## =============================================================
## generate_kolkata_data.R
## Synthetic POS-style dataset for "Rongin Rasoi" -- a Bengali /
## North Indian / Indo-Chinese restaurant in Gariahat, Kolkata.
## Writes clean_sales_data.csv (already included; re-run only if
## you want a fresh random sample).
## =============================================================
set.seed(42)
suppressPackageStartupMessages({ library(dplyr); library(lubridate); library(tibble) })

menu <- tribble(
  ~Product_Name,             ~Category,                ~Base_Price, ~Popularity,
  "Chicken Kosha",           "Main Course (Non-Veg)",   240,  9,
  "Mutton Kosha",            "Main Course (Non-Veg)",   320,  5,
  "Chingri Malai Curry",     "Main Course (Non-Veg)",   300,  3,
  "Bhetki Fish Curry",       "Main Course (Non-Veg)",   260,  4,
  "Egg Curry",               "Main Course (Non-Veg)",   130,  4,
  "Aloo Posto",              "Main Course (Veg)",       110,  4,
  "Shukto",                  "Main Course (Veg)",       120,  2,
  "Dal Makhani",             "Main Course (Veg)",       140,  5,
  "Paneer Butter Masala",    "Main Course (Veg)",       180,  6,
  "Mixed Veg Curry",         "Main Course (Veg)",       130,  3,
  "Kolkata Chicken Biryani", "Biryani & Rice",          220, 12,
  "Mutton Biryani",          "Biryani & Rice",          280,  7,
  "Egg Biryani",             "Biryani & Rice",          150,  4,
  "Veg Fried Rice",          "Biryani & Rice",          130,  3,
  "Plain Rice",              "Biryani & Rice",           60,  3,
  "Chicken Pakora",          "Starters",                150,  5,
  "Fish Fry",                "Starters",                180,  6,
  "Mutton Seekh Kebab",      "Starters",                220,  3,
  "Veg Cutlet",              "Starters",                 90,  3,
  "Chilli Paneer (Starter)", "Starters",                160,  4,
  "Luchi (4pc)",             "Breads",                   60,  6,
  "Butter Naan",             "Breads",                   45,  6,
  "Tandoori Roti",           "Breads",                   25,  5,
  "Kulcha",                  "Breads",                   45,  2,
  "Paratha",                 "Breads",                   35,  3,
  "Chilli Chicken",          "Indo-Chinese",            200,  7,
  "Chow Mein",               "Indo-Chinese",            140,  5,
  "Chicken Manchurian",      "Indo-Chinese",            150,  4,
  "Hakka Noodles",           "Indo-Chinese",            150,  4,
  "Masala Chai",             "Beverages",                30, 10,
  "Lassi",                   "Beverages",                60,  5,
  "Cold Coffee",             "Beverages",                90,  5,
  "Lemon Soda",              "Beverages",                40,  4,
  "Mineral Water",           "Beverages",                20,  6,
  "Rasgulla (2pc)",          "Desserts",                 50,  4,
  "Mishti Doi",              "Desserts",                 60,  4,
  "Sandesh",                 "Desserts",                 70,  2,
  "Gulab Jamun (2pc)",       "Desserts",                 60,  3
)

date_range <- seq(as.Date("2023-06-01"), as.Date("2023-12-31"), by = "day")
wd_abbr    <- as.character(wday(date_range, label = TRUE, abbr = TRUE, week_start = 1))
durga_puja <- seq(as.Date("2023-10-15"), as.Date("2023-10-24"), by = "day")
kali_puja  <- as.Date("2023-11-12")
monsoon    <- seq(as.Date("2023-07-01"), as.Date("2023-08-31"), by = "day")

day_mult <- function(d) {
  wd <- wday(d, week_start = 1)
  base <- if (wd %in% c(6, 7)) 1.35 else if (wd == 5) 1.15 else 1.0
  if (d %in% durga_puja) base <- base * 1.9
  if (d == kali_puja)    base <- base * 1.6
  if (d %in% monsoon)    base <- base * 0.88
  base
}

hour_weights <- c(`11` = 0.4, `12` = 1.4, `13` = 1.6, `14` = 1.0, `15` = 0.3,
                  `16` = 0.2, `17` = 0.3, `18` = 0.6, `19` = 1.3, `20` = 1.8,
                  `21` = 1.7, `22` = 1.1, `23` = 0.4)

rows <- vector("list", 40000); i <- 1; BASE_LAMBDA <- 2.8
for (d_i in seq_along(date_range)) {
  d <- date_range[d_i]; dm <- day_mult(d)
  for (h in as.integer(names(hour_weights))) {
    hw <- hour_weights[as.character(h)]
    n_orders <- rpois(1, max(0.3, BASE_LAMBDA * dm * hw))
    if (n_orders == 0) next
    for (o in seq_len(n_orders)) {
      n_items   <- sample(1:4, 1, prob = c(0.35, 0.35, 0.2, 0.1))
      items_idx <- sample(seq_len(nrow(menu)), n_items, prob = menu$Popularity)
      cust_type <- sample(c("Dine-in", "Takeaway", "Delivery (Swiggy)", "Delivery (Zomato)"),
                          1, prob = c(0.45, 0.15, 0.22, 0.18))
      pay_mode  <- if (grepl("Delivery", cust_type)) "UPI" else
                   sample(c("Cash", "Card", "UPI"), 1, prob = c(0.3, 0.25, 0.45))
      for (idx in items_idx) {
        qty      <- sample(1:3, 1, prob = c(0.7, 0.22, 0.08))
        price    <- round(menu$Base_Price[idx] * runif(1, 0.95, 1.08))
        gross    <- price * qty
        discount <- if (runif(1) < 0.06) round(gross * 0.10) else 0
        tax      <- round(gross * 0.05, 2)           # 5% GST
        total    <- round(gross - discount + tax, 2)
        event    <- if (runif(1) < 0.01) "Refund" else "Payment"
        rows[[i]] <- data.frame(
          Order_Date = d, Weekday = wd_abbr[d_i], Order_Hour = h,
          Store_Branch = "Rongin Rasoi - Gariahat", City = "Kolkata, West Bengal",
          Product_Name = menu$Product_Name[idx], Category = menu$Category[idx],
          Quantity_Sold = qty, Unit_Price = price, Discount = discount,
          Tax_Amount = tax, Total_Sales = ifelse(event == "Refund", -total, total),
          Customer_Type = cust_type, Payment_Mode = pay_mode, Event_Type = event)
        i <- i + 1
      }
    }
  }
}
clean_sales <- bind_rows(rows[seq_len(i - 1)])
write.csv(clean_sales, "clean_sales_data.csv", row.names = FALSE)
cat("Rows:", nrow(clean_sales), " -> saved clean_sales_data.csv\n")
