# Data layer: turns a symbol list into an aligned wide xts of adjusted closes.
# This is the only file that touches the network; everything downstream is
# source-agnostic, so the indicator math and plotting can be driven by any
# price matrix (CSV, database, synthetic test data) just as well.

# Download adjusted close prices for `symbols` and return them merged into one
# wide xts (one column per symbol), aligned on dates where all symbols traded.
# Symbols that fail to download are dropped with a warning.
fetch_prices <- function(symbols, from, to) {
  series <- lapply(symbols, function(sym) {
    tryCatch({
      raw <- quantmod::getSymbols(sym, src = "yahoo", from = from, to = to,
                                  auto.assign = FALSE)
      adj <- quantmod::Ad(raw)
      colnames(adj) <- sym
      adj
    }, error = function(e) {
      warning(sprintf("Failed to download %s: %s", sym, conditionMessage(e)),
              call. = FALSE)
      NULL
    })
  })
  series <- Filter(Negate(is.null), series)
  if (length(series) == 0) {
    stop("No price data could be downloaded.")
  }
  stats::na.omit(do.call(merge, series))
}

# Reduce a daily price matrix to weekly bars (last observation of each week).
# The final bar covers the week containing `as_of` (usually today);
# `complete_only` drops it so a mid-week run doesn't treat a partial week as a
# weekly close. Passing `as_of` explicitly keeps the function deterministic.
to_weekly <- function(prices, complete_only = FALSE, as_of = Sys.Date()) {
  weekly <- prices[xts::endpoints(prices, on = "weeks"), ]
  if (complete_only && nrow(weekly) > 0) {
    last_week <- format(utils::tail(zoo::index(weekly), 1), "%G-%V")
    if (last_week == format(as.Date(as_of), "%G-%V")) {
      weekly <- weekly[-nrow(weekly), ]
    }
  }
  weekly
}
