test_that("module server registers summary and table outputs", {
  input_df <- data.frame(
    value = c(1, 2, NA, 4),
    group = c("a", "b", "a", NA),
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    data_viewer_server,
    args = list(
      id = "viewer",
      data = shiny::reactive(input_df)
    ),
    {
      summary_html <- output$summary_panel$html
      table_payload <- as.character(output$data_table)

      expect_type(output$summary_panel, "list")
      expect_match(summary_html, "de-var-card")
      expect_match(summary_html, "value")
      expect_match(summary_html, "group")
      expect_match(table_payload, "value")
      expect_match(table_payload, "group")
    }
  )
})

test_that("module server updates rendered outputs when data changes", {
  data_state <- shiny::reactiveVal(
    data.frame(
      value = c(1, 2, 3),
      stringsAsFactors = FALSE
    )
  )

  shiny::testServer(
    data_viewer_server,
    args = list(
      id = "viewer",
      data = shiny::reactive(data_state())
    ),
    {
      initial_summary <- output$summary_panel$html
      initial_payload <- as.character(output$data_table)

      expect_match(initial_summary, "value")
      expect_no_match(initial_summary, "flag")
      expect_match(initial_payload, "value")
      expect_no_match(initial_payload, "flag")

      data_state(
        data.frame(
          value = c(1, 2, 3),
          flag = c(TRUE, FALSE, TRUE),
          stringsAsFactors = FALSE
        )
      )

      session$flushReact()

      updated_summary <- output$summary_panel$html
      updated_payload <- as.character(output$data_table)

      expect_match(updated_summary, "flag")
      expect_match(updated_payload, "value")
      expect_match(updated_payload, "flag")
    }
  )
})

test_that("validate_data_frame rejects invalid inputs", {
  expect_error(validate_data_frame(NULL), "Data is not available")
  expect_error(validate_data_frame(list(a = 1)), "`data` must be a data.frame", fixed = TRUE)
  expect_error(validate_data_frame(data.frame()), "Dataset must contain at least one column")
})

test_that("validate_summary_card_fn rejects invalid inputs", {
  expect_error(validate_summary_card_fn("not a function"), "`summary_card_fn` must be a function.", fixed = TRUE)
})

test_that("module server respects top_n and table customization hooks", {
  input_df <- data.frame(
    category = c("a", "b", "c", "d", "a", "b", "e"),
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    data_viewer_server,
    args = list(
      id = "viewer",
      data = shiny::reactive(input_df),
      top_n = 2,
      default_page_size = 5,
      page_size_options = c(5, 10, 20),
      searchable = FALSE,
      filterable = FALSE,
      sortable = FALSE,
      reactable_args = list(
        showPageInfo = FALSE
      )
    ),
    {
      summary_html <- output$summary_panel$html
      table_payload <- as.character(output$data_table)

      expect_match(summary_html, "Other")
      expect_match(table_payload, "\"defaultPageSize\":5")
      expect_match(table_payload, "\"pageSizeOptions\":\\[5,10,20\\]")
      expect_no_match(table_payload, "\"searchable\":true")
      expect_no_match(table_payload, "\"filterable\":true")
      expect_no_match(table_payload, "\"sortable\":true")
      expect_match(table_payload, "\"showPageInfo\":false")
    }
  )
})

test_that("module server uses custom summary card function", {
  input_df <- data.frame(
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  custom_summary_card <- function(summary_row, index) {
    htmltools::tags$div(
      class = "custom-summary-card",
      sprintf("%s-%s", index, summary_row$var_name[[1]])
    )
  }

  shiny::testServer(
    data_viewer_server,
    args = list(
      id = "viewer",
      data = shiny::reactive(input_df),
      summary_card_fn = custom_summary_card
    ),
    {
      summary_html <- output$summary_panel$html

      expect_match(summary_html, "custom-summary-card")
      expect_match(summary_html, "1-value")
      expect_no_match(summary_html, "de-var-card")
    }
  )
})
