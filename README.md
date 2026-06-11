# relative-rotation-graph

Relative Rotation Graph (RRG): plots JdK RS-Ratio vs. JdK RS-Momentum trails
for a set of symbols against a benchmark, on weekly data.

The exact JdK formulas are proprietary, so this uses the standard public
approximation:

- `RS = 100 * price / benchmark`
- `RS-Ratio = 100 + rolling z-score of WMA(RS, smooth)`
- `RS-Momentum = 100 + rolling z-score of WMA(ROC(RS-Ratio), smooth)`

The `smooth` WMA pass is a trailing trend filter that mimics the smoothing in
the licensed JdK indicators (the ones on Bloomberg terminals); it is what makes
trails arc instead of zigzag. Set `smooth = 1` to disable it. All filtering and
normalization uses trailing windows only (no lookahead), so each point on a
trail is what you would have seen in real time. Quadrants are centered at
(100, 100): Leading, Weakening, Lagging, Improving.

## Usage

```sh
Rscript main.r          # fetch data, print quadrant summary, save rrg.png
Rscript tests/test-rrg.R  # offline unit tests on synthetic data
```

Symbols, benchmark, and indicator parameters live in the `config` block at the
top of `main.r`.

## Structure

| Path                | Layer                                                          |
|---------------------|----------------------------------------------------------------|
| `R/data.R`          | Data: Yahoo download + weekly conversion (only network code)   |
| `R/rrg.R`           | Domain: indicator math, `rrg()` constructor, `print` method    |
| `R/plot.R`          | Presentation: `rrg_plot()` / `plot` method for an `rrg` object |
| `tests/test-rrg.R`  | Offline tests: quadrant semantics, no-lookahead, rotation      |
| `main.r`            | Entry point: config + calls                                    |

The layers compose, so the parts are usable on their own:

```r
for (f in list.files("R", full.names = TRUE)) source(f)

prices <- to_weekly(fetch_prices(c("XLK", "XLF", "SPY"),
                                 from = Sys.Date() - 730, to = Sys.Date()))
g <- rrg(prices, benchmark = "SPY")   # any wide xts works, not just Yahoo data
print(g)                              # current quadrant per symbol
plot(g)                               # draw the chart
```

`rrg()` is frequency-agnostic — `window`, `roc_period`, `trail_len`, and
`smooth` (defaults 14 / 4 / 10 / 1) are in bars of whatever frequency the input
carries. More smoothing means rounder trails but later signals: the filter
delays quadrant crossings, which is also true of the licensed indicator
(StockCharts documents roughly ten weeks between a relative-strength peak and
the RS-Ratio centerline cross).

## Requirements

R with `quantmod`, `ggplot2`, and `ggrepel`:

```r
install.packages(c("quantmod", "ggplot2", "ggrepel"))
```

The code also uses `xts`, `TTR`, and `zoo` directly; these are dependencies of
`quantmod` and are installed along with it.

The tests need only `xts`/`TTR`/`zoo` (installed with quantmod) and `ggplot2` —
no network access.
