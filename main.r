# Load required libraries
library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(xts)
library(zoo)
library(ggrepel)
library(TTR)

# Define the calculate_rrg_metrics function
calculate_rrg_metrics <- function(prices, benchmark, n = 63) {
  # Ensure prices and benchmark are numeric vectors
  prices <- as.numeric(prices)
  benchmark <- as.numeric(benchmark)
  
  # Calculate returns
  returns <- diff(log(prices))
  bench_returns <- diff(log(benchmark))
  
  # Calculate relative strength
  rs <- cumprod(1 + (returns - bench_returns))
  
  # Calculate simple moving average
  sma <- function(x, n) {
    stats::filter(x, rep(1/n, n), sides = 1)
  }
  
  # Calculate JdK RS-Ratio
  rs_mean <- sma(rs, n)
  rs_ratio <- (rs / rs_mean) - 1
  rs_ratio <- rs_ratio * 100
  
  # Calculate JdK RS-Momentum
  rs_momentum <- (rs / lag(rs, n)) - 1
  rs_momentum <- rs_momentum * 100
  
  # Remove NA values
  valid_rows <- complete.cases(rs_ratio, rs_momentum)
  rs_ratio <- rs_ratio[valid_rows]
  rs_momentum <- rs_momentum[valid_rows]
  
  # Normalize RS-Ratio and RS-Momentum
  rs_ratio <- (rs_ratio - mean(rs_ratio, na.rm = TRUE)) / sd(rs_ratio, na.rm = TRUE)
  rs_momentum <- (rs_momentum - mean(rs_momentum, na.rm = TRUE)) / sd(rs_momentum, na.rm = TRUE)
  
  # Scale to -100 to 100 range
  rs_ratio <- pmin(pmax(rs_ratio * 20, -100), 100)
  rs_momentum <- pmin(pmax(rs_momentum * 20, -100), 100)
  
  # Combine results
  result <- data.frame(rs_ratio = rs_ratio,
                       rs_momentum = rs_momentum)
  
  return(result)
}

# Code to download and prepare data (unchanged)
symbols <- c("QQQ", "XLV", "XLE", "VNQ")
benchmark <- "SPY"

# Download data
for (sym in c(symbols, benchmark)) {
  tryCatch({
    getSymbols(sym, from = "2023-08-07", to = "2024-07-22", auto.assign = TRUE)
    cat("Successfully downloaded data for", sym, "\n")
  }, error = function(e) {
    cat("Failed to download data for", sym, "\nError:", conditionMessage(e), "\n")
  })
}

# Prepare data (as before)
end_date <- Sys.Date()
start_date <- end_date - 365

# Create an empty xts object to store prices
prices <- do.call(merge, lapply(symbols, function(sym) {
  if (exists(sym)) {
    data <- Ad(get(sym))
    data <- data[paste0(start_date, "/", end_date)]
    return(data)
  }
  return(NULL)
}))

cat("Structure of prices object:\n")
print(str(prices))
cat("\nFirst few rows of prices:\n")
print(head(prices))

if (ncol(prices) == 0) {
  stop("No price data available for any symbol")
}

if (!exists(benchmark)) {
  stop("Benchmark data not available")
}

bench_prices <- Ad(get(benchmark))
bench_prices <- bench_prices[paste0(start_date, "/", end_date)]

cat("\nStructure of benchmark prices:\n")
print(str(bench_prices))
cat("\nFirst few rows of benchmark prices:\n")
print(head(bench_prices))

# Align prices and benchmark data
aligned_data <- merge(prices, bench_prices)
aligned_data <- aligned_data[complete.cases(aligned_data),]

cat("\nStructure of aligned data:\n")
print(str(aligned_data))
cat("\nFirst few rows of aligned data:\n")
print(head(aligned_data))

# Calculate RRG metrics for each symbol
rrg_data <- lapply(1:(ncol(aligned_data)-1), function(i) {
  cat("\nCalculating metrics for", colnames(aligned_data)[i], "\n")
  metrics <- calculate_rrg_metrics(aligned_data[,i], aligned_data[,ncol(aligned_data)])
  if (!is.null(metrics) && nrow(metrics) > 0) {
    metrics$symbol <- colnames(aligned_data)[i]
    metrics$date <- index(aligned_data)[(nrow(aligned_data) - nrow(metrics) + 1):nrow(aligned_data)]
    cat("Sample of metrics for", colnames(aligned_data)[i], ":\n")
    print(head(metrics))
    return(metrics)
  }
  return(NULL)
})

# Combine data and convert to data frame
rrg_df <- do.call(rbind, rrg_data)

# Check structure before further processing
if (!is.null(rrg_df) && nrow(rrg_df) > 0) {
  cat("\nStructure of rrg_df:\n")
  print(str(rrg_df))
  cat("\nFirst few rows of rrg_df:\n")
  print(head(rrg_df))
  
  # Remove any remaining rows with NA values
  rrg_df <- rrg_df[complete.cases(rrg_df),]
  
  # Print information about the final dataset
  cat("\nFinal dataset summary:\n")
  print(summary(rrg_df))
  cat("\nNumber of rows per symbol:\n")
  print(table(rrg_df$symbol))
  
  # Plotting
  rrg_df$symbol <- as.factor(rrg_df$symbol)

  # Use smoother lines and adjust arrow types
  p <- ggplot(rrg_df, aes(x = rs_ratio, y = rs_momentum, color = symbol)) +
    geom_path(arrow = arrow(length = unit(0.1, "inches"), type = "closed"), linewidth = 0.8) +  # Adjust line width
    geom_point(data = rrg_df %>% group_by(symbol) %>% slice_tail(n = 1), shape = 21, size = 4, fill = "white") +
    geom_point(data = rrg_df %>% group_by(symbol) %>% slice_head(n = 1), shape = 24, size = 3, fill = "black") +
    geom_text_repel(data = rrg_df %>% group_by(symbol) %>% slice_tail(n = 1), 
                    aes(label = format(as.Date(date), "%m-%d")),
                    size = 3, box.padding = 0.5, point.padding = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Relative Rotation Graph",
         x = "JdK RS-Ratio", 
         y = "JdK RS-Momentum") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    coord_cartesian(xlim = c(-40, 40), ylim = c(-40, 40))  # Adjusted axis limits for better spacing
  
  p <- p +
    annotate("text", x = 12, y = 12, label = "Leading", fontface = "bold") +
    annotate("text", x = -12, y = 12, label = "Improving", fontface = "bold") +
    annotate("text", x = -12, y = -12, label = "Lagging", fontface = "bold") +
    annotate("text", x = 12, y = -12, label = "Weakening", fontface = "bold")
  
  print(p)
} else {
  cat("No valid RRG data available for plotting.\n")
}
