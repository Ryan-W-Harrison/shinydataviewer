test_that("module UI builders attach one viewer dependency and namespace outputs", {
  ui <- data_viewer_ui("viewer")
  deps <- htmltools::findDependencies(ui)

  expect_equal(sum(vapply(deps, function(x) identical(x$name, "shinydataviewer"), logical(1))), 1L)

  ui_text <- paste(as.character(ui), collapse = "\n")
  expect_match(ui_text, "viewer-data_table")
  expect_match(ui_text, "viewer-summary_panel")
  expect_no_match(ui_text, "card-header\">Data")
  expect_no_match(ui_text, "sidebar-title\">Variables")
  expect_match(ui_text, "de-root--controls-top")
})

test_that("card wrapper includes header and embedded table region", {
  ui <- data_viewer_card_ui("embedded", title = "Embedded Viewer", full_screen = FALSE)
  deps <- htmltools::findDependencies(ui)

  expect_equal(sum(vapply(deps, function(x) identical(x$name, "shinydataviewer"), logical(1))), 1L)

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

test_that("variable summary cards expose tooltip-enabled chart bars", {
  summary_df <- summarize_columns(data.frame(category = c("a", "b", "a"), stringsAsFactors = FALSE))
  card <- variable_summary_card(summary_df[1, , drop = FALSE], 1)
  card_text <- paste(as.character(card), collapse = "\n")

  expect_match(card_text, "de-mini-chart__bar-wrap")
  expect_match(card_text, "title=\"Value: &#39;a&#39;")
  expect_match(card_text, "Details")
})
