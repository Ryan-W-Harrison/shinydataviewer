#' Data Viewer UI Helpers
#'
#' Small reusable UI components for the data viewer module.
#'
#' @noRd
missing_bar <- function(pct_missing) {
  if (!is.finite(pct_missing) || pct_missing <= 0) {
    return(NULL)
  }

  htmltools::tags$div(
    class = "de-missing-bar",
    htmltools::tags$div(
      class = "de-missing-bar__fill",
      style = sprintf("width:%s%%;", round(100 * pct_missing, 1))
    )
  )
}

#' @noRd
mini_histogram <- function(distribution) {
  bins <- distribution$bins %||% numeric()
  ranges <- distribution$ranges
  total <- distribution$total %||% sum(bins)
  value_type <- distribution$value_type %||% "numeric"
  scaled <- scale_bars(bins)

  if (length(scaled) == 0) {
    return(empty_distribution("No data"))
  }

  htmltools::tags$div(
    class = "de-mini-chart de-mini-chart--hist",
    lapply(
      seq_along(scaled),
      function(i) {
        mini_chart_bar(
          height = scaled[[i]],
          tooltip = histogram_tooltip(
            left = ranges$left[[i]],
            right = ranges$right[[i]],
            count = bins[[i]],
            total = total,
            value_type = value_type
          )
        )
      }
    )
  )
}

#' @noRd
mini_bar_chart <- function(distribution) {
  counts <- distribution$counts

  if (is.null(counts) || nrow(counts) == 0) {
    return(empty_distribution("No levels"))
  }

  scaled <- scale_bars(counts$count)
  total <- distribution$total %||% sum(counts$count)

  htmltools::tags$div(
    class = "de-mini-chart de-mini-chart--bars",
    lapply(
      seq_len(nrow(counts)),
      function(i) {
        mini_chart_bar(
          height = scaled[[i]],
          tooltip = categorical_tooltip(
            value = counts$level[[i]],
            count = counts$count[[i]],
            total = total
          )
        )
      }
    )
  )
}

#' @noRd
variable_summary_card <- function(summary_row, index) {
  distribution <- summary_row$distribution_data[[1]]
  stats <- summary_row$summary_stats[[1]]
  mini_plot <- if (identical(distribution$kind, "histogram")) {
    mini_histogram(distribution)
  } else {
    mini_bar_chart(distribution)
  }

  bslib::card(
    class = "de-var-card",
    bslib::card_body(
      class = "de-var-card__body",
      htmltools::tags$div(
        class = "de-var-card__header",
        htmltools::tags$div(
          class = "de-var-card__identity",
          htmltools::tags$div(
            class = "de-var-card__name",
            summary_row$var_name[[1]]
          ),
          htmltools::tags$div(
            class = "de-var-card__type",
            summary_row$type[[1]]
          )
        ),
        htmltools::tags$div(
          class = "de-var-card__missing",
          htmltools::tags$span(class = "de-muted-label", "Missing"),
          htmltools::tags$strong(format_pct(summary_row$pct_missing[[1]]))
        )
      ),
      missing_bar(summary_row$pct_missing[[1]]),
      htmltools::tags$div(
        class = "de-var-card__distribution",
        mini_plot
      )
    ),
    bslib::accordion(
      id = sprintf("var-details-%s", index),
      open = FALSE,
      multiple = TRUE,
      bslib::accordion_panel(
        title = "Details",
        detail_panel(summary_row$type[[1]], stats)
      )
    )
  )
}

#' @noRd
detail_panel <- function(type, stats) {
  if (type == "numeric") {
    rows <- list(
      detail_row("Missing", format_count(stats$missing)),
      detail_row("Min", format_number(stats$min)),
      detail_row("Q1", format_number(stats$q1)),
      detail_row("Median", format_number(stats$median)),
      detail_row("Q3", format_number(stats$q3)),
      detail_row("Max", format_number(stats$max)),
      detail_row("Mean", format_number(stats$mean)),
      detail_row("SD", format_number(stats$sd))
    )

    return(htmltools::tags$div(
      class = "de-detail-grid",
      htmltools::tagList(rows)
    ))
  }

  if (type == "date") {
    rows <- list(
      detail_row("Missing", format_count(stats$missing)),
      detail_row("Min", format_date(stats$min)),
      detail_row("Median", format_date(stats$median)),
      detail_row("Max", format_date(stats$max))
    )

    return(htmltools::tags$div(
      class = "de-detail-grid",
      htmltools::tagList(rows)
    ))
  }

  top_levels <- stats$top_levels
  level_rows <- if (nrow(top_levels) == 0) {
    htmltools::tags$div(class = "de-detail-empty", "No non-missing levels")
  } else {
    lapply(
      seq_len(nrow(top_levels)),
      function(i) {
        detail_row(
          top_levels$level[[i]],
          format_count_pct(top_levels$count[[i]], top_levels$pct[[i]])
        )
      }
    )
  }

  htmltools::tagList(
    htmltools::tags$div(
      class = "de-detail-grid",
      detail_row("Missing", format_count(stats$missing)),
      detail_row("Unique", format_count(stats$n_unique))
    ),
    htmltools::tags$div(class = "de-detail-section-label", "Top levels"),
    htmltools::tags$div(
      class = "de-detail-grid",
      htmltools::tagList(level_rows)
    )
  )
}

#' @noRd
detail_row <- function(label, value) {
  htmltools::tags$div(
    class = "de-detail-row",
    htmltools::tags$span(class = "de-detail-row__label", label),
    htmltools::tags$span(class = "de-detail-row__value", value)
  )
}

#' @noRd
empty_distribution <- function(label) {
  htmltools::tags$div(class = "de-distribution-empty", label)
}

#' @noRd
mini_chart_bar <- function(height, tooltip) {
  htmltools::tags$button(
    type = "button",
    class = "de-mini-chart__bar-wrap",
    tabindex = "0",
    `aria-label` = gsub("\n", " ", tooltip, fixed = TRUE),
    title = tooltip,
    htmltools::tags$span(
      class = "de-mini-chart__bar",
      style = sprintf("height:%s%%;", height)
    )
  )
}

#' @noRd
scale_bars <- function(values) {
  if (length(values) == 0) {
    return(numeric())
  }

  max_value <- max(values, na.rm = TRUE)

  if (!is.finite(max_value) || max_value <= 0) {
    return(rep(0, length(values)))
  }

  pmax((values / max_value) * 100, 8)
}

#' @noRd
format_pct <- function(x) {
  sprintf("%.1f%%", 100 * x)
}

#' @noRd
format_count <- function(x) {
  format(x, big.mark = ",", trim = TRUE)
}

#' @noRd
format_count_pct <- function(count, pct) {
  sprintf("%s (%s)", format_count(count), format_pct(pct))
}

#' @noRd
format_number <- function(x) {
  if (is.na(x)) {
    return("NA")
  }

  format(round(x, 2), nsmall = 2, trim = TRUE, big.mark = ",")
}

#' @noRd
format_date <- function(x) {
  if (is.na(x)) {
    return("NA")
  }

  format(x, "%Y-%m-%d")
}

#' @noRd
categorical_tooltip <- function(value, count, total) {
  sprintf(
    "Value: '%s'\nCount: %s (%s)",
    value,
    format_count(count),
    format_pct(safe_share(count, total))
  )
}

#' @noRd
histogram_tooltip <- function(
  left,
  right,
  count,
  total,
  value_type = "numeric"
) {
  sprintf(
    "Range: %s to %s\nCount: %s (%s)",
    format_range_value(left, value_type),
    format_range_value(right, value_type),
    format_count(count),
    format_pct(safe_share(count, total))
  )
}

#' @noRd
format_range_value <- function(x, value_type = "numeric") {
  if (inherits(x, "Date") || identical(value_type, "date")) {
    return(format_date(as.Date(x, origin = "1970-01-01")))
  }

  format_number(as.numeric(x))
}

#' @noRd
safe_share <- function(x, total) {
  if (is.null(total) || is.na(total) || total <= 0) {
    return(0)
  }

  x / total
}

#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
