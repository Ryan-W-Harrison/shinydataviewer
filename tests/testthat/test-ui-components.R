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
