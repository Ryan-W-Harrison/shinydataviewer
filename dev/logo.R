# Dev script to generate the package hex logo.
# Not used at runtime.

library(ggplot2)
library(hexSticker)

viewer_palette <- list(
  bg = "#F7F9FC",
  panel = "#FFFFFF",
  panel_alt = "#EEF4FA",
  accent = "#2A93D5",
  accent_dark = "#0F766E",
  accent_light = "#BFE3F7",
  grid = "#D5E0EA",
  text = "#17364A",
  muted = "#6F8193",
  border = "#D7E1EA"
)

bars_top <- data.frame(
  xmin = c(0.13, 0.185, 0.24, 0.295, 0.35),
  xmax = c(0.175, 0.23, 0.285, 0.34, 0.385),
  ymin = 0.62,
  ymax = c(0.635, 0.635, 0.635, 0.635, 0.705),
  fill = c("#74BDE6", "#74BDE6", "#74BDE6", "#74BDE6", "#2A93D5")
)

bars_mid <- data.frame(
  xmin = c(0.13, 0.185, 0.24, 0.295, 0.35),
  xmax = c(0.175, 0.23, 0.285, 0.34, 0.385),
  ymin = 0.38,
  ymax = c(0.435, 0.49, 0.455, 0.405, 0.425),
  fill = c("#6CB7E2", "#2A93D5", "#4BA7DC", "#77C0E7", "#5BAEE0")
)

bars_bot <- data.frame(
  xmin = c(0.13, 0.225, 0.32),
  xmax = c(0.215, 0.31, 0.405),
  ymin = 0.14,
  ymax = c(0.205, 0.19, 0.165),
  fill = c("#2A93D5", "#46A3DB", "#73BFE6")
)

table_header <- expand.grid(row = 1, col = seq_len(6))
table_header$xmin <- 0.49 + (table_header$col - 1) * 0.064
table_header$xmax <- table_header$xmin + 0.051
table_header$ymin <- 0.60
table_header$ymax <- 0.645

table_filters <- expand.grid(row = 1, col = seq_len(6))
table_filters$xmin <- 0.49 + (table_filters$col - 1) * 0.064
table_filters$xmax <- table_filters$xmin + 0.051
table_filters$ymin <- 0.545
table_filters$ymax <- 0.585

table_rows <- expand.grid(row = seq_len(6), col = seq_len(6))
table_rows$xmin <- 0.49 + (table_rows$col - 1) * 0.064
table_rows$xmax <- table_rows$xmin + 0.051
table_rows$ymax <- 0.505 - (table_rows$row - 1) * 0.052
table_rows$ymin <- table_rows$ymax - 0.036

table_rule_y <- c(0.53, 0.478, 0.426, 0.374, 0.322, 0.270, 0.218)

build_icon_plot <- function(simple = FALSE) {
  if (simple) {
    return(
      ggplot() +
        annotate(
          "rect",
          xmin = 0.13,
          xmax = 0.42,
          ymin = 0.18,
          ymax = 0.78,
          fill = viewer_palette$panel,
          color = viewer_palette$border,
          linewidth = 0.55
        ) +
        annotate(
          "rect",
          xmin = 0.48,
          xmax = 0.86,
          ymin = 0.18,
          ymax = 0.78,
          fill = viewer_palette$panel,
          color = viewer_palette$border,
          linewidth = 0.55
        ) +
        annotate(
          "rect",
          xmin = 0.445,
          xmax = 0.458,
          ymin = 0.18,
          ymax = 0.78,
          fill = viewer_palette$grid,
          color = NA
        ) +
        geom_rect(
          data = bars_mid,
          aes(xmin = xmin, xmax = xmax, ymin = ymin - 0.02, ymax = ymax + 0.06),
          fill = bars_mid$fill,
          color = NA
        ) +
        geom_rect(
          data = table_rows[table_rows$row <= 4 & table_rows$col <= 5, ],
          aes(
            xmin = xmin + 0.01,
            xmax = xmax + 0.005,
            ymin = ymin + 0.02,
            ymax = ymax + 0.08
          ),
          fill = viewer_palette$panel,
          color = viewer_palette$grid,
          linewidth = 0.3
        ) +
        annotate(
          "rect",
          xmin = 0.50,
          xmax = 0.80,
          ymin = 0.70,
          ymax = 0.745,
          fill = viewer_palette$panel_alt,
          color = viewer_palette$border,
          linewidth = 0.3
        ) +
        coord_equal(
          xlim = c(0, 1),
          ylim = c(0, 1),
          expand = FALSE,
          clip = "off"
        ) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = viewer_palette$bg, color = NA)
        )
    )
  }

  ggplot() +
    annotate(
      "rect",
      xmin = 0.10,
      xmax = 0.41,
      ymin = 0.09,
      ymax = 0.80,
      fill = viewer_palette$panel,
      color = viewer_palette$border,
      linewidth = 0.5
    ) +
    annotate(
      "rect",
      xmin = 0.47,
      xmax = 0.90,
      ymin = 0.09,
      ymax = 0.80,
      fill = viewer_palette$panel,
      color = viewer_palette$border,
      linewidth = 0.5
    ) +
    annotate(
      "rect",
      xmin = 0.43,
      xmax = 0.445,
      ymin = 0.09,
      ymax = 0.80,
      fill = viewer_palette$grid,
      color = NA
    ) +
    annotate(
      "rect",
      xmin = 0.885,
      xmax = 0.897,
      ymin = 0.12,
      ymax = 0.77,
      fill = viewer_palette$grid,
      color = NA
    ) +
    annotate(
      "rect",
      xmin = 0.885,
      xmax = 0.897,
      ymin = 0.42,
      ymax = 0.56,
      fill = viewer_palette$muted,
      color = NA
    ) +
    annotate(
      "rect",
      xmin = 0.50,
      xmax = 0.84,
      ymin = 0.72,
      ymax = 0.765,
      fill = viewer_palette$panel_alt,
      color = viewer_palette$border,
      linewidth = 0.35
    ) +
    annotate(
      "segment",
      x = 0.525,
      xend = 0.80,
      y = 0.742,
      yend = 0.742,
      color = viewer_palette$muted,
      linewidth = 0.7,
      lineend = "round"
    ) +
    geom_rect(
      data = bars_top,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = bars_top$fill,
      color = NA
    ) +
    geom_rect(
      data = bars_mid,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = bars_mid$fill,
      color = NA
    ) +
    geom_rect(
      data = bars_bot,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = bars_bot$fill,
      color = NA
    ) +
    annotate(
      "segment",
      x = c(0.13, 0.13, 0.13),
      xend = c(0.22, 0.19, 0.20),
      y = c(0.75, 0.725, 0.51),
      yend = c(0.75, 0.725, 0.51),
      color = c(viewer_palette$text, viewer_palette$muted, viewer_palette$text),
      linewidth = c(1.1, 0.7, 1.1),
      lineend = "round"
    ) +
    annotate(
      "segment",
      x = c(0.13, 0.13, 0.13),
      xend = c(0.20, 0.19, 0.18),
      y = c(0.485, 0.27, 0.245),
      yend = c(0.485, 0.27, 0.245),
      color = c(
        viewer_palette$muted,
        viewer_palette$text,
        viewer_palette$muted
      ),
      linewidth = c(0.7, 1.1, 0.7),
      lineend = "round"
    ) +
    geom_rect(
      data = table_header,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = viewer_palette$panel_alt,
      color = viewer_palette$grid,
      linewidth = 0.28
    ) +
    geom_rect(
      data = table_filters,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = viewer_palette$panel,
      color = viewer_palette$border,
      linewidth = 0.25
    ) +
    geom_rect(
      data = table_rows,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = viewer_palette$panel,
      color = viewer_palette$grid,
      linewidth = 0.22
    ) +
    annotate(
      "segment",
      x = 0.49,
      xend = 0.88,
      y = table_rule_y,
      yend = table_rule_y,
      color = viewer_palette$grid,
      linewidth = 0.25
    ) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = viewer_palette$bg, color = NA)
    )
}

primary_plot <- build_icon_plot(simple = FALSE)
small_plot <- build_icon_plot(simple = TRUE)

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

hexSticker::sticker(
  subplot = primary_plot,
  package = "shinydataviewer",
  p_family = "sans",
  p_color = viewer_palette$text,
  p_size = 5.5,
  p_y = 1.31,
  h_fill = viewer_palette$bg,
  h_color = viewer_palette$text,
  h_size = 1.15,
  s_width = 1.88,
  s_x = 1.01,
  s_y = 1.02,
  filename = "man/figures/logo.png"
)

hexSticker::sticker(
  subplot = primary_plot,
  package = "shinydataviewer",
  p_family = "sans",
  p_color = viewer_palette$text,
  p_size = 5.5,
  p_y = 1.31,
  h_fill = viewer_palette$bg,
  h_color = viewer_palette$text,
  h_size = 1.15,
  s_width = 1.88,
  s_x = 1.01,
  s_y = 1.02,
  filename = "man/figures/logo.svg"
)

hexSticker::sticker(
  subplot = small_plot,
  package = "",
  p_family = "sans",
  p_color = viewer_palette$text,
  p_size = 1,
  h_fill = viewer_palette$bg,
  h_color = viewer_palette$text,
  h_size = 1.15,
  s_width = 2.05,
  s_x = 1.01,
  s_y = 1.07,
  filename = "man/figures/logo-small.png"
)

hexSticker::sticker(
  subplot = small_plot,
  package = "",
  p_family = "sans",
  p_color = viewer_palette$text,
  p_size = 1,
  h_fill = viewer_palette$bg,
  h_color = viewer_palette$text,
  h_size = 1.15,
  s_width = 2.05,
  s_x = 1.01,
  s_y = 1.07,
  filename = "man/figures/logo-small.svg"
)
