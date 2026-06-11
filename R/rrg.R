# Domain layer: pure indicator math and the `rrg` object.
#
# The exact JdK formulas are proprietary; this is the standard public
# approximation:
#
#   RS          = 100 * price / benchmark
#   RS-Ratio    = 100 + rolling z-score of RS          (relative trend)
#   RS-Momentum = 100 + rolling z-score of ROC(RS-Ratio)
#
# All normalization is rolling (trailing window only), so a point computed at
# bar t never changes as later data arrives. Indicators are frequency-agnostic:
# `window`, `roc_period`, and `trail_len` are in bars of whatever frequency the
# input carries (canonically weekly).

rolling_zscore <- function(x, window) {
  z <- (x - TTR::runMean(x, window)) / TTR::runSD(x, window)
  z[is.nan(z) | is.infinite(z)] <- NA  # flat windows have sd 0; yield NA, not Inf
  z
}

# Validate a tuning parameter as a single finite whole number >= min and
# return it as integer, failing fast with a clear message instead of erroring
# deep inside TTR or producing a nonsensical row requirement.
as_count <- function(x, min, name) {
  if (length(x) != 1 || !is.numeric(x) || !is.finite(x) || x %% 1 != 0 ||
      x < min) {
    stop("`", name, "` must be a single whole number >= ", min, ".")
  }
  as.integer(x)
}

# Trailing weighted moving average (linear weights, most recent bar heaviest).
# n = 1 is the identity, so unsmoothed and smoothed share one code path.
wma <- function(x, n) {
  n <- as_count(n, 1L, "smooth")
  if (n == 1L) return(x)
  TTR::WMA(x, n)
}

# Pure function: two aligned price series in, xts of (rs_ratio, rs_momentum) out.
# `smooth` low-pass filters the RS line (and the momentum input) with a
# trailing WMA before normalization, mimicking the trend-following filter in
# the licensed JdK indicators. Higher = smoother trails, more lag.
calc_rrg_metrics <- function(price, bench, window, roc_period, smooth = 1) {
  rs <- 100 * price / bench

  # Where the (filtered) relative-strength line sits within its recent range,
  # in trailing standard deviations, centered at 100
  rs_ratio <- 100 + rolling_zscore(wma(rs, smooth), window)

  # Rate of change of RS-Ratio (its derivative drives the clockwise rotation),
  # filtered and normalized the same way
  roc <- 100 * (rs_ratio / xts::lag.xts(rs_ratio, roc_period) - 1)
  rs_momentum <- 100 + rolling_zscore(wma(roc, smooth), window)

  out <- merge(rs_ratio, rs_momentum)
  colnames(out) <- c("rs_ratio", "rs_momentum")
  out
}

# Quadrant of a point relative to the (100, 100) center. Vectorized.
quadrant <- function(rs_ratio, rs_momentum) {
  ifelse(rs_ratio >= 100,
         ifelse(rs_momentum >= 100, "Leading", "Weakening"),
         ifelse(rs_momentum >= 100, "Improving", "Lagging"))
}

# Construct an RRG from a wide xts of prices. `benchmark` names the benchmark
# column; every other column becomes a trail. Returns an object of class "rrg"
# holding the trails and the parameters that produced them.
rrg <- function(prices, benchmark, window = 14, roc_period = 4, trail_len = 10,
                smooth = 1) {
  stopifnot(xts::is.xts(prices))
  window <- as_count(window, 2L, "window")
  roc_period <- as_count(roc_period, 1L, "roc_period")
  trail_len <- as_count(trail_len, 1L, "trail_len")
  smooth <- as_count(smooth, 1L, "smooth")
  if (!benchmark %in% colnames(prices)) {
    stop("Benchmark column '", benchmark, "' not found in prices.")
  }
  symbols <- setdiff(colnames(prices), benchmark)
  if (length(symbols) == 0) {
    stop("Prices must contain at least one non-benchmark column.")
  }

  # rs_ratio needs `window` bars, its ROC another `roc_period`, the momentum
  # z-score another `window`, each WMA pass adds `smooth - 1`, and the trail
  # sits on top of that
  min_rows <- 2L * window + roc_period + trail_len + 2L * (smooth - 1L)
  if (nrow(prices) < min_rows) {
    stop(sprintf("Need at least %d rows for window=%d, roc_period=%d, trail_len=%d, smooth=%d; got %d.",
                 min_rows, window, roc_period, trail_len, smooth, nrow(prices)))
  }

  trails <- do.call(rbind, lapply(symbols, function(sym) {
    m <- calc_rrg_metrics(prices[, sym], prices[, benchmark], window, roc_period,
                          smooth)
    m <- utils::tail(stats::na.omit(m), trail_len)
    if (nrow(m) == 0) return(NULL)
    data.frame(date = zoo::index(m), symbol = sym, zoo::coredata(m),
               row.names = NULL)
  }))
  if (is.null(trails) || nrow(trails) == 0) {
    stop("No valid RRG observations could be computed.")
  }

  structure(
    list(trails = trails,
         benchmark = benchmark,
         params = list(window = window, roc_period = roc_period,
                       trail_len = trail_len, smooth = smooth)),
    class = "rrg"
  )
}

# Latest observation per symbol, with its quadrant.
rrg_heads <- function(x) {
  stopifnot(inherits(x, "rrg"))
  h <- do.call(rbind, lapply(split(x$trails, x$trails$symbol), utils::tail, 1))
  h$quadrant <- quadrant(h$rs_ratio, h$rs_momentum)
  rownames(h) <- NULL
  h
}

print.rrg <- function(x, ...) {
  h <- rrg_heads(x)
  cat(sprintf("Relative Rotation Graph vs %s — as of %s\n",
              x$benchmark, max(x$trails$date)))
  cat(sprintf("window = %d, roc_period = %d, smooth = %d, trail = %d bars\n\n",
              x$params$window, x$params$roc_period, x$params$smooth,
              x$params$trail_len))
  h$rs_ratio <- round(h$rs_ratio, 2)
  h$rs_momentum <- round(h$rs_momentum, 2)
  print(h[, c("symbol", "rs_ratio", "rs_momentum", "quadrant")],
        row.names = FALSE)
  invisible(x)
}
