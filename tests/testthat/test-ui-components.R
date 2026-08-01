test_that("module UI builders attach one viewer dependency and namespace outputs", {
  ui <- data_viewer_ui("viewer")
  deps <- htmltools::findDependencies(ui)

  expect_equal(
    sum(vapply(
      deps,
      function(x) identical(x$name, "shinydataviewer"),
      logical(1)
    )),
    1L
  )

  ui_text <- paste(as.character(ui), collapse = "\n")
  expect_match(ui_text, "viewer-data_table")
  expect_match(ui_text, "viewer-summary_panel")
  expect_no_match(ui_text, "card-header\">Data")
  expect_no_match(ui_text, "sidebar-title\">Variables")
  expect_match(ui_text, "de-root--controls-top")
})

test_that("card wrapper includes header and embedded table region", {
  ui <- data_viewer_card_ui(
    "embedded",
    title = "Embedded Viewer",
    full_screen = FALSE
  )
  deps <- htmltools::findDependencies(ui)

  expect_equal(
    sum(vapply(
      deps,
      function(x) identical(x$name, "shinydataviewer"),
      logical(1)
    )),
    1L
  )

  ui_text <- paste(as.character(ui), collapse = "\n")
  expect_match(ui_text, "Embedded Viewer")
  expect_match(ui_text, "embedded-data_table")
  expect_match(ui_text, "de-module-card")
  expect_match(ui_text, "de-root--controls-top")
})

test_that("module UI supports bottom-positioned table controls", {
  ui <- data_viewer_ui("viewer", table_controls_position = "bottom")
  ui_text <- paste(as.character(ui), collapse = "\n")

  expect_match(ui_text, "de-root--controls-bottom")
  expect_no_match(ui_text, "de-root--controls-top")
})

test_that("UI helpers scope custom plot colors to the viewer root", {
  colors <- c(
    "#2c7fb8",
    "rebeccapurple",
    "rgb(44 127 184)",
    "var(--bs-success)"
  )

  for (color in colors) {
    module_ui <- paste(
      as.character(data_viewer_ui("module", plot_color = color)),
      collapse = "\n"
    )
    card_ui <- paste(
      as.character(data_viewer_card_ui("card", plot_color = color)),
      collapse = "\n"
    )

    expected_style <- paste0("--de-plot-color:", color, ";")
    expect_match(module_ui, expected_style, fixed = TRUE)
    expect_match(card_ui, expected_style, fixed = TRUE)
  }

  default_ui <- paste(as.character(data_viewer_ui("default")), collapse = "\n")
  expect_no_match(default_ui, "--de-plot-color", fixed = TRUE)
})

test_that("plot colors do not leak between viewer roots", {
  first <- paste(
    as.character(data_viewer_ui("first", plot_color = "red")),
    collapse = "\n"
  )
  second <- paste(
    as.character(data_viewer_ui("second", plot_color = "blue")),
    collapse = "\n"
  )

  expect_match(first, "--de-plot-color:red;", fixed = TRUE)
  expect_no_match(first, "--de-plot-color:blue;", fixed = TRUE)
  expect_match(second, "--de-plot-color:blue;", fixed = TRUE)
  expect_no_match(second, "--de-plot-color:red;", fixed = TRUE)
})

test_that("plot color validation rejects invalid and unsafe values", {
  invalid_values <- list(
    character(),
    c("red", "blue"),
    1,
    TRUE,
    NA_character_,
    "",
    "   "
  )

  for (value in invalid_values) {
    expect_error(
      data_viewer_ui("viewer", plot_color = value),
      "`plot_color` must be NULL or a non-empty CSS color string.",
      fixed = TRUE
    )
  }

  unsafe_values <- c("red;color:black", "red{color:black}", "red\nblue")

  for (value in unsafe_values) {
    expect_error(
      data_viewer_card_ui("viewer", plot_color = value),
      "`plot_color` must not contain CSS declaration delimiters.",
      fixed = TRUE
    )
  }
})

test_that("default table styles use Bootstrap color-mode variables", {
  theme <- default_reactable_theme()

  expect_identical(theme$borderColor, "var(--bs-border-color)")
  expect_match(theme$stripedColor, "var\\(--bs-body-bg\\)")
  expect_match(theme$stripedColor, "var\\(--bs-body-color\\)")
  expect_identical(theme$style$color, "var(--bs-body-color)")
  expect_identical(theme$style$backgroundColor, "var(--bs-body-bg)")

  stylesheet <- paste(
    readLines(file.path(data_viewer_www_path(), "data-viewer.css")),
    collapse = "\n"
  )

  expect_match(stylesheet, "\\.de-root \\.Reactable")
  expect_match(stylesheet, "\\.de-root \\.rt-search")
  expect_match(stylesheet, "background-color: var\\(--bs-body-bg\\)")
  expect_match(stylesheet, "color: var\\(--bs-body-color\\)")
  expect_match(stylesheet, "--de-plot-color: var\\(--bs-primary\\)")
  expect_match(
    stylesheet,
    "color-mix\\(in srgb, var\\(--de-plot-color\\) 95%, transparent\\)"
  )
  expect_match(
    stylesheet,
    paste0(
      "\\.de-root \\.de-module-card \\{[^}]*",
      "background: var\\(--bs-card-bg, var\\(--bs-body-bg\\)\\)"
    )
  )
})

test_that("variable summary cards expose tooltip-enabled chart bars", {
  summary_df <- summarize_columns(data.frame(
    category = c("a", "b", "a"),
    stringsAsFactors = FALSE
  ))
  card <- variable_summary_card(summary_df[1, , drop = FALSE], 1)
  card_text <- paste(as.character(card), collapse = "\n")

  expect_match(card_text, "de-mini-chart__bar-wrap")
  expect_match(card_text, "title=\"Value: &#39;a&#39;")
  expect_match(card_text, "Details")
})
