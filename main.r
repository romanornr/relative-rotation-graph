#!/usr/bin/env Rscript
# Entry point: configure, fetch, build, render. All logic lives in R/.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  "."
}
for (f in list.files(file.path(root, "R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(f)
}

config <- list(
  symbols    = c("QQQ", "XLV", "XLE", "VNQ"),
  benchmark  = "SPY",
  window     = 14,  # rolling normalization window, in weekly bars
  roc_period = 4,   # RS-Ratio momentum lookback, in weekly bars
  trail_len  = 10,  # weekly observations shown per trail
  smooth     = 10,  # WMA trend filter on RS and momentum (1 = off)
  years_back = 2,   # history to download
  output     = "rrg.png"
)

end_date   <- Sys.Date()
start_date <- end_date - round(365.25 * config$years_back)

prices <- to_weekly(fetch_prices(c(config$symbols, config$benchmark),
                                 from = start_date, to = end_date),
                    complete_only = TRUE)

g <- rrg(prices,
         benchmark  = config$benchmark,
         window     = config$window,
         roc_period = config$roc_period,
         trail_len  = config$trail_len,
         smooth     = config$smooth)

print(g)

ggplot2::ggsave(config$output, rrg_plot(g), width = 9, height = 9, dpi = 150)
cat("\nSaved plot to", config$output, "\n")
