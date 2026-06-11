# Presentation layer: renders an `rrg` object. Owns the ggplot2/ggrepel
# dependency; nothing here computes indicators.

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
})

# Build (and return) the ggplot for an rrg object. The caller decides what to
# do with it: print it, ggsave() it, or add layers.
rrg_plot <- function(x) {
  stopifnot(inherits(x, "rrg"))
  trails <- x$trails
  heads <- rrg_heads(x)

  # Symmetric limits around the (100, 100) center so quadrants stay square
  lim <- max(abs(c(trails$rs_ratio, trails$rs_momentum) - 100)) * 1.2
  lim <- max(lim, 1.5)

  quadrants <- data.frame(
    xmin  = c(100, 100 - lim, 100 - lim, 100),
    xmax  = c(100 + lim, 100, 100, 100 + lim),
    ymin  = c(100, 100, 100 - lim, 100 - lim),
    ymax  = c(100 + lim, 100 + lim, 100, 100),
    label = c("Leading", "Improving", "Lagging", "Weakening"),
    fill  = c("#00b05033", "#0070c033", "#c0000033", "#bf900033")
  )

  ggplot(trails, aes(x = rs_ratio, y = rs_momentum, color = symbol)) +
    geom_rect(data = quadrants, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = fill)) +
    geom_text(data = quadrants, inherit.aes = FALSE,
              aes(x = (xmin + xmax) / 2, y = ifelse(ymin < 100, ymin, ymax),
                  label = label, vjust = ifelse(ymin < 100, -0.5, 1.5)),
              fontface = "bold", color = "grey40", size = 3.5) +
    scale_fill_identity() +
    geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 100, linetype = "dashed", color = "grey50") +
    geom_path(aes(group = symbol),
              arrow = arrow(length = unit(0.12, "inches"), type = "closed"),
              linewidth = 0.8) +
    geom_point(size = 1.6) +
    geom_point(data = heads, size = 3.5) +
    geom_text_repel(data = heads, aes(label = symbol),
                    fontface = "bold", size = 4, show.legend = FALSE,
                    box.padding = 0.6, point.padding = 0.4) +
    scale_color_brewer(palette = "Set1") +
    # Default breaks step by 2 across this narrow a range, which leaves only
    # three labeled gridlines inside the clipped window; ask for finer ticks
    scale_x_continuous(n.breaks = 8) +
    scale_y_continuous(n.breaks = 8) +
    coord_equal(xlim = 100 + c(-lim, lim), ylim = 100 + c(-lim, lim),
                expand = FALSE) +
    labs(title = "Relative Rotation Graph",
         subtitle = sprintf("vs %s, %d-bar trail (as of %s)",
                            x$benchmark, x$params$trail_len, max(trails$date)),
         x = "JdK RS-Ratio", y = "JdK RS-Momentum", color = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

plot.rrg <- function(x, y, ...) {
  p <- rrg_plot(x)
  print(p)
  invisible(p)
}
