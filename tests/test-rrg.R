# Offline unit tests for the RRG domain layer, on synthetic price data.
# No network, no quantmod — that's the point of the layering.
#
# Run with: Rscript tests/test-rrg.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
} else {
  "."
}
for (f in list.files(file.path(root, "R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(f)
}

failures <- 0L
check <- function(desc, expr) {
  ok <- isTRUE(tryCatch(expr, error = function(e) {
    message("  error: ", conditionMessage(e))
    FALSE
  }))
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", desc))
  if (!ok) failures <<- failures + 1L
}

# ---- Synthetic data ----------------------------------------------------------
# BENCH drifts up steadily; OUT outperforms it every bar, UNDER underperforms
# every bar, CYC cycles around it with a 52-bar period.

n <- 200
dates <- seq(as.Date("2020-01-03"), by = "week", length.out = n)
bench <- 100 * cumprod(rep(1.001, n))
px <- xts::xts(
  cbind(OUT   = bench * cumprod(rep(1.002, n)),
        UNDER = bench * cumprod(rep(0.998, n)),
        CYC   = bench * (1 + 0.05 * sin(2 * pi * seq_len(n) / 52)),
        BENCH = bench),
  order.by = dates
)

g <- rrg(px, benchmark = "BENCH")
h <- rrg_heads(g)

# ---- Object contract ---------------------------------------------------------

check("rrg() returns an object of class 'rrg'", inherits(g, "rrg"))
check("trails have the expected columns",
      identical(colnames(g$trails), c("date", "symbol", "rs_ratio", "rs_momentum")))
check("each symbol's trail has trail_len observations",
      all(table(g$trails$symbol) == g$params$trail_len))
check("trail dates are increasing within each symbol",
      all(tapply(as.numeric(g$trails$date), g$trails$symbol,
                 function(d) all(diff(d) > 0))))
check("rrg() errors when the benchmark column is missing",
      inherits(tryCatch(rrg(px, benchmark = "NOPE"), error = function(e) e),
               "error"))
check("rrg() errors on insufficient history",
      inherits(tryCatch(rrg(px[1:20, ], benchmark = "BENCH"),
                        error = function(e) e),
               "error"))
check("rrg() rejects malformed tuning parameters",
      all(vapply(list(list(smooth = 10.5), list(smooth = Inf),
                      list(smooth = NA), list(window = c(14, 20)),
                      list(roc_period = 0), list(trail_len = "10")),
                 function(bad) {
                   args <- c(list(px, benchmark = "BENCH"), bad)
                   inherits(tryCatch(do.call(rrg, args),
                                     error = function(e) e),
                            "error")
                 }, logical(1))))

# ---- Quadrant semantics ------------------------------------------------------

check("quadrant() maps the four corners correctly",
      identical(quadrant(c(101, 101, 99, 99), c(101, 99, 101, 99)),
                c("Leading", "Weakening", "Improving", "Lagging")))
check("a steady outperformer sits right of center (RS-Ratio > 100)",
      h$rs_ratio[h$symbol == "OUT"] > 100)
check("a steady underperformer sits left of center (RS-Ratio < 100)",
      h$rs_ratio[h$symbol == "UNDER"] < 100)

# ---- No lookahead ------------------------------------------------------------
# The value at bar t must be identical whether or not later data exists.
# This is the regression test for the original full-sample normalization bug.

m_full <- calc_rrg_metrics(px[, "CYC"], px[, "BENCH"], window = 14, roc_period = 4)
check("metrics at bar t never change as later data arrives",
      all(vapply(c(60, 120, 180), function(k) {
        m_part <- calc_rrg_metrics(px[1:k, "CYC"], px[1:k, "BENCH"],
                                   window = 14, roc_period = 4)
        isTRUE(all.equal(zoo::coredata(m_full[1:k, ]), zoo::coredata(m_part)))
      }, logical(1))))

# ---- Rotation ----------------------------------------------------------------
# RS-Momentum is the derivative of RS-Ratio, so on a cyclical relative-strength
# series it must turn down before RS-Ratio does. That phase lead is what makes
# trails rotate clockwise.

first_downcross <- function(v, after) {
  prev <- v[-length(v)]
  curr <- v[-1]
  idx <- which(!is.na(prev) & !is.na(curr) & prev >= 100 & curr < 100) + 1L
  idx <- idx[idx > after]
  if (length(idx) == 0L) NA_integer_ else idx[1L]
}

ratio_cross <- first_downcross(as.numeric(m_full$rs_ratio), after = 40)
mom_cross <- first_downcross(as.numeric(m_full$rs_momentum), after = 40)
check("RS-Momentum crosses below 100 before RS-Ratio (clockwise rotation)",
      !is.na(ratio_cross) && !is.na(mom_cross) &&
        mom_cross < ratio_cross && (ratio_cross - mom_cross) < 26)

# ---- Smoothing -----------------------------------------------------------------
# The WMA trend filter must reduce trail jerkiness, must not introduce
# lookahead, and smooth = 1 must be a no-op.

set.seed(42)
noisy <- bench * (1 + 0.05 * sin(2 * pi * seq_len(n) / 52) + rnorm(n, sd = 0.02))
px_noisy <- xts::xts(cbind(NOISY = noisy, BENCH = bench), order.by = dates)

jerk <- function(v) mean(abs(diff(diff(v))), na.rm = TRUE)
m_raw <- calc_rrg_metrics(px_noisy[, "NOISY"], px_noisy[, "BENCH"],
                          window = 14, roc_period = 4)
m_smooth <- calc_rrg_metrics(px_noisy[, "NOISY"], px_noisy[, "BENCH"],
                             window = 14, roc_period = 4, smooth = 10)
check("smoothing reduces RS-Ratio trail jerkiness",
      jerk(as.numeric(m_smooth$rs_ratio)) < jerk(as.numeric(m_raw$rs_ratio)))
check("smoothing reduces RS-Momentum trail jerkiness",
      jerk(as.numeric(m_smooth$rs_momentum)) < jerk(as.numeric(m_raw$rs_momentum)))

check("smooth = 1 is the default and a no-op",
      isTRUE(all.equal(
        zoo::coredata(calc_rrg_metrics(px[, "CYC"], px[, "BENCH"],
                                       window = 14, roc_period = 4, smooth = 1)),
        zoo::coredata(m_full))))

check("smoothed metrics at bar t never change as later data arrives",
      all(vapply(c(120, 160), function(k) {
        m_part <- calc_rrg_metrics(px_noisy[1:k, "NOISY"], px_noisy[1:k, "BENCH"],
                                   window = 14, roc_period = 4, smooth = 10)
        isTRUE(all.equal(zoo::coredata(m_smooth[1:k, ]), zoo::coredata(m_part)))
      }, logical(1))))

# ---- Degenerate inputs ---------------------------------------------------------
# A symbol that tracks the benchmark exactly has zero variance in its RS line;
# the z-score denominator is 0 and must yield NA, never Inf or NaN.

px_flat <- xts::xts(cbind(FLAT = bench, BENCH = bench), order.by = dates)
m_flat <- calc_rrg_metrics(px_flat[, "FLAT"], px_flat[, "BENCH"],
                           window = 14, roc_period = 4)
check("flat relative strength yields NA, never Inf or NaN",
      !any(is.infinite(zoo::coredata(m_flat)) | is.nan(zoo::coredata(m_flat))))

# ---- Weekly conversion ---------------------------------------------------------
# complete_only must drop the partial bar for the week containing as_of.
# A fixed as_of keeps this deterministic: 2020-06-10 was a Wednesday.

wednesday <- as.Date("2020-06-10")
daily <- xts::xts(seq_len(40), order.by = seq(wednesday - 39, wednesday, by = "day"))
wk <- to_weekly(daily, complete_only = TRUE, as_of = wednesday)
check("to_weekly(complete_only = TRUE) drops the in-progress week",
      nrow(wk) > 0 &&
        format(utils::tail(zoo::index(wk), 1), "%G-%V") != format(wednesday, "%G-%V"))
check("to_weekly() keeps the in-progress week by default",
      format(utils::tail(zoo::index(to_weekly(daily, as_of = wednesday)), 1), "%G-%V") ==
        format(wednesday, "%G-%V"))

# ---- Presentation ------------------------------------------------------------

check("rrg_plot() returns a ggplot object",
      inherits(rrg_plot(g), c("gg", "ggplot", "ggplot2::ggplot")))

# ------------------------------------------------------------------------------

cat("\n")
if (failures > 0L) {
  cat(failures, "test(s) failed\n")
  quit(status = 1L)
}
cat("All tests passed\n")
