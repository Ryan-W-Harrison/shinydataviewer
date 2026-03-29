expect_matches_snapshot_file <- function(actual, filename) {
  snapshot_path <- testthat::test_path("snapshots", filename)
  expected <- paste(readLines(snapshot_path, warn = FALSE), collapse = "\n")
  expect_identical(actual, expected)
}

test_that("variable summary card markup remains stable", {
  summary_df <- summarize_columns(
    data.frame(
      category = c("a", "b", "a"),
      stringsAsFactors = FALSE
    )
  )

  html <- normalize_snapshot_html(as.character(variable_summary_card(summary_df[1, , drop = FALSE], 1)))

  expect_matches_snapshot_file(html, "variable-summary-card.html")
})

test_that("standalone module UI markup remains stable", {
  html <- normalize_snapshot_html(as.character(data_viewer_ui("viewer")))

  expect_matches_snapshot_file(html, "module-ui.html")
})

test_that("bottom-controls module UI markup remains stable", {
  html <- normalize_snapshot_html(
    as.character(data_viewer_ui("viewer", table_controls_position = "bottom"))
  )

  expect_matches_snapshot_file(html, "module-ui-bottom-controls.html")
})

test_that("embedded card UI markup remains stable", {
  html <- normalize_snapshot_html(
    as.character(data_viewer_card_ui("embedded", title = "Embedded Viewer", full_screen = FALSE))
  )

  expect_matches_snapshot_file(html, "embedded-card-ui.html")
})
