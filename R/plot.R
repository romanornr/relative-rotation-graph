# Presentation layer: renders an `rrg` object. Owns the ggplot2/ggrepel
# dependency; nothing here computes indicators.

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
})

# Dark terminal palette
RRG_BG     <- "#0E1117"
RRG_GRID   <- "#1E2530"
RRG_AXIS   <- "#8B949E"
RRG_CENTER <- "#3A4356"
RRG_TEXT   <- "#C9D1D9"
RRG_TITLE  <- "#F0F6FC"
RRG_COLORS <- c("#FF5C8A", "#19D3F3", "#FFC25C", "#7CF29C",
                "#B388FF", "#FF8A50", "#62D2C3", "#F06FEE")

# Build (and return) the ggplot for an rrg object. The caller decides what to
# do with it: print it, ggsave() it, or add layers.
rrg_plot <- function(x) {
  stopifnot(inherits(x, "rrg"))
  trails <- x$trails
  heads <- rrg_heads(x)

  # Recency within each trail, 0 = oldest bar, 1 = head. Trails fade and
  # thin toward the past so the direction of travel reads without arrows.
  trails$frac <- ave(seq_len(nrow(trails)), trails$symbol,
                     FUN = function(i) {
                       (seq_along(i) - 1) / max(1L, length(i) - 1L)
                     })

  # Symmetric limits around the (100, 100) center so quadrants stay square
  lim <- max(abs(c(trails$rs_ratio, trails$rs_momentum) - 100)) * 1.2
  lim <- max(lim, 1.5)

  # Quadrant names as faint corner watermarks instead of filled rectangles
  inset <- lim * 0.04
  watermarks <- data.frame(
    x     = c(100 + lim - inset, 100 - lim + inset,
              100 - lim + inset, 100 + lim - inset),
    y     = c(100 + lim - inset, 100 + lim - inset,
              100 - lim + inset, 100 - lim + inset),
    hjust = c(1, 0, 0, 1),
    vjust = c(1, 1, 0, 0),
    label = c("LEADING", "IMPROVING", "LAGGING", "WEAKENING"),
    color = c("#00E676", "#00E5FF", "#FF5252", "#FFD740")
  )

  n_sym <- length(unique(trails$symbol))

  # Soft glow behind the watermarks: the same text stamped at small offsets
  # with low alpha reads as a halo on the dark background
  delta <- lim * 0.008
  offsets <- expand.grid(dx = -1:1, dy = -1:1)
  offsets <- offsets[offsets$dx != 0 | offsets$dy != 0, ]
  glow_layers <- lapply(seq_len(nrow(offsets)), function(i) {
    d <- watermarks
    d$x <- d$x + offsets$dx[i] * delta
    d$y <- d$y + offsets$dy[i] * delta
    geom_text(data = d, inherit.aes = FALSE,
              aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
              color = d$color, alpha = 0.07,
              fontface = "bold", size = 4.6)
  })

  ggplot(trails, aes(x = rs_ratio, y = rs_momentum, color = symbol)) +
    glow_layers +
    geom_text(data = watermarks, inherit.aes = FALSE,
              aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
              color = watermarks$color, alpha = 0.9,
              fontface = "bold", size = 4.6) +
    geom_hline(yintercept = 100, linetype = "dashed",
               color = RRG_CENTER, linewidth = 0.4) +
    geom_vline(xintercept = 100, linetype = "dashed",
               color = RRG_CENTER, linewidth = 0.4) +
    geom_path(aes(group = symbol, alpha = frac), linewidth = 0.9) +
    geom_point(aes(alpha = frac, size = frac)) +
    geom_point(data = heads, size = 6, alpha = 0.25) +
    geom_point(data = heads, size = 2.8) +
    geom_text_repel(data = heads, aes(label = symbol),
                    fontface = "bold", size = 4.2, show.legend = FALSE,
                    box.padding = 0.6, point.padding = 0.4, seed = 7) +
    scale_color_manual(values = if (n_sym <= length(RRG_COLORS)) {
      RRG_COLORS[seq_len(n_sym)]
    } else {
      # interpolate so colors stay distinct beyond the base palette
      grDevices::colorRampPalette(RRG_COLORS)(n_sym)
    }) +
    scale_alpha(range = c(0.12, 1)) +
    scale_size(range = c(0.5, 2.2)) +
    # Default breaks step by 2 across this narrow a range, which leaves only
    # three labeled gridlines inside the clipped window; ask for finer ticks
    scale_x_continuous(n.breaks = 8) +
    scale_y_continuous(n.breaks = 8) +
    coord_equal(xlim = 100 + c(-lim, lim), ylim = 100 + c(-lim, lim),
                expand = FALSE) +
    labs(title = "Relative Rotation Graph",
         subtitle = sprintf("vs %s, %d-bar trail (as of %s)",
                            x$benchmark, x$params$trail_len, max(trails$date)),
         x = "JdK RS-Ratio", y = "JdK RS-Momentum") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none",
          plot.background = element_rect(fill = RRG_BG, color = NA),
          panel.background = element_rect(fill = RRG_BG, color = NA),
          panel.grid.major = element_line(color = RRG_GRID, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          text = element_text(color = RRG_TEXT),
          axis.text = element_text(color = RRG_AXIS),
          axis.title = element_text(color = RRG_AXIS),
          plot.title = element_text(color = RRG_TITLE, face = "bold"),
          plot.subtitle = element_text(color = RRG_AXIS))
}

plot.rrg <- function(x, y, ...) {
  p <- rrg_plot(x)
  print(p)
  invisible(p)
}
