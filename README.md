# R_MiniProject
Wine quality normalisation - Wine quality datatset
# SUPERSTORE SALES TREND ANALYSIS
superstore <- read.csv("C:\\Users\\Khushi Tanwar\\Downloads\\archive (2)\\SuperStoreOrders.csv")

cat("Data loaded! Rows:", nrow(superstore), "\n")
cat("\nColumn names in dataset:\n")
print(names(superstore))
cat("\nSample sales values (first 5):\n")
print(head(superstore$sales))
superstore$sales <- gsub(",", "", superstore$sales)  # Remove commas like "1,648" -> "1648"
superstore$sales <- as.numeric(superstore$sales)     # Convert to number
na_count <- sum(is.na(superstore$sales))
if(na_count > 0) {
  cat("Warning:", na_count, "sales values could not be converted\n")
}

if("profit" %in% names(superstore)) {
  superstore$profit <- gsub(",", "", superstore$profit)
  superstore$profit <- as.numeric(superstore$profit)
}

superstore$order_date <- as.Date(superstore$order_date, format = "%d/%m/%Y")

if(all(is.na(superstore$order_date))) {
  superstore$order_date <- as.Date(superstore$order_date, format = "%m/%d/%Y")
}
if(all(is.na(superstore$order_date))) {
  superstore$order_date <- as.Date(superstore$order_date, format = "%Y-%m-%d")
}

before_rows <- nrow(superstore)
superstore <- superstore[!is.na(superstore$order_date), ]
after_rows <- nrow(superstore)
cat("Rows removed due to invalid dates:", before_rows - after_rows, "\n")
superstore$year <- format(superstore$order_date, "%Y")
superstore$month <- format(superstore$order_date, "%m")
superstore$month_name <- format(superstore$order_date, "%b")
monthly_agg <- aggregate(sales ~ year + month_name + month, 
                         data = superstore, 
                         FUN = sum, na.rm = TRUE)
monthly_agg$month_num <- as.numeric(monthly_agg$month)
monthly_agg <- monthly_agg[order(monthly_agg$year, monthly_agg$month_num), ]
years <- unique(monthly_agg$year)
years <- years[!is.na(years)]
colors <- c("blue", "red", "green", "purple", "orange", "brown")
plot(1:12, type = "n", 
     xlab = "Month", ylab = "Total Sales ($)",
     main = "Monthly Sales Trend by Year",
     xaxt = "n",
     xlim = c(1, 12),
     ylim = c(0, max(monthly_agg$sales, na.rm = TRUE) * 1.1))

axis(1, at = 1:12, labels = month.abb)
i <- 1
for(yr in years) {
  yr_data <- monthly_agg[monthly_agg$year == yr, ]
  if(nrow(yr_data) > 0) {
    yr_data <- yr_data[order(yr_data$month_num), ]
    lines(1:nrow(yr_data), yr_data$sales, 
          col = colors[i], lwd = 2, type = "b", pch = 16)
    i <- i + 1
  }
}
legend("topright", legend = years, col = colors[1:(i-1)], 
       lwd = 2, title = "Year")
category_agg <- aggregate(sales ~ category, data = superstore, FUN = sum, na.rm = TRUE)
category_agg <- category_agg[order(category_agg$sales, decreasing = TRUE), ]

category_agg <- category_agg[!is.na(category_agg$category), ]

barplot(category_agg$sales, 
        names.arg = category_agg$category, 
        col = c("skyblue", "lightgreen", "salmon"),
        main = "Sales by Product Category",
        xlab = "Category", ylab = "Total Sales ($)",
        ylim = c(0, max(category_agg$sales, na.rm = TRUE) * 1.1))

text(x = 1:nrow(category_agg), 
     y = category_agg$sales + max(category_agg$sales, na.rm = TRUE)*0.02, 
     labels = paste0("$", round(category_agg$sales/1000, 1), "K"))

subcat_agg <- aggregate(sales ~ sub_category, data = superstore, FUN = sum, na.rm = TRUE)
subcat_agg <- subcat_agg[order(subcat_agg$sales, decreasing = TRUE), ]
subcat_agg <- subcat_agg[!is.na(subcat_agg$sub_category), ]
top10 <- head(subcat_agg, 10)

par(mar = c(5, 12, 4, 4))
barplot(top10$sales, 
        names.arg = top10$sub_category, 
        col = rainbow(10),
        main = "Top 10 Sub-Categories by Sales",
        xlab = "Total Sales ($)", 
        horiz = TRUE,
        las = 1,
        cex.names = 0.8)
par(mar = c(5, 4, 4, 2) + 0.1)

ship_agg <- aggregate(sales ~ ship_mode, data = superstore, FUN = sum, na.rm = TRUE)
ship_agg <- ship_agg[!is.na(ship_agg$ship_mode), ]
pie(ship_agg$sales, 
    labels = paste0(ship_agg$ship_mode, "\n", round(ship_agg$sales/sum(ship_agg$sales, na.rm = TRUE)*100, 1), "%"),
    main = "Sales Distribution by Shipping Mode",
    col = c("lightblue", "lightgreen", "lightsalmon", "lightyellow"))

cat("\n==========================================\n")
cat("ANALYSIS RESULTS\n")
cat("==========================================\n")

total_sales <- sum(superstore$sales, na.rm = TRUE)
cat("Total Sales: $", format(round(total_sales, 0), big.mark = ","), "\n")
if(nrow(category_agg) > 0) {
  cat("\nBest Category:", category_agg$category[1], 
      "- $", format(round(category_agg$sales[1], 0), big.mark = ","), "\n")
}
if(nrow(top10) > 0) {
  cat("Best Sub-Category:", top10$sub_category[1], 
      "- $", format(round(top10$sales[1], 0), big.mark = ","), "\n")
}
monthly_total <- aggregate(sales ~ year + month_name, data = superstore, FUN = sum, na.rm = TRUE)
monthly_total <- monthly_total[order(monthly_total$sales, decreasing = TRUE), ]
if(nrow(monthly_total) > 0) {
  best_month <- monthly_total[1, ]
  cat("Best Month:", best_month$month_name, best_month$year, 
      "- $", format(round(best_month$sales, 0), big.mark = ","), "\n")
}
if(nrow(ship_agg) > 0) {
  best_ship <- ship_agg[which.max(ship_agg$sales), ]
  cat("Most Used Ship Mode:", best_ship$ship_mode, "\n")
}
yearly_agg <- aggregate(sales ~ year, data = superstore, FUN = sum, na.rm = TRUE)
yearly_agg <- yearly_agg[order(yearly_agg$year), ]
cat("\nYearly Sales:\n")
for(i in 1:nrow(yearly_agg)) {
  if(!is.na(yearly_agg$year[i])) {
    cat("  ", yearly_agg$year[i], ": $", 
        format(round(yearly_agg$sales[i], 0), big.mark = ","), "\n")
  }
}
if("country" %in% names(superstore)) {
  countries <- unique(superstore$country)
  countries <- countries[!is.na(countries)]
  cat("\nNumber of Countries:", length(countries), "\n")
}
if("customer_name" %in% names(superstore)) {
  customers <- unique(superstore$customer_name)
  customers <- customers[!is.na(customers)]
  cat("Number of Customers:", length(customers), "\n")
}

cat("\n✅ Analysis Complete! All plots displayed.\n")

