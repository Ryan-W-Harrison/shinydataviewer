test_that("summarize_columns returns expected metadata for mixed data", {
  df <- data.frame(
    value = c(1, 2, NA, 4),
    group = c("a", "b", "a", NA),
    flag = c(TRUE, FALSE, TRUE, NA),
    observed_on = as.Date("2025-01-01") + c(0, 1, NA, 3),
    stringsAsFactors = FALSE
  )

  summary_df <- summarize_columns(df, top_n = 1)

  expect_equal(nrow(summary_df), 4)
  expect_equal(unname(summary_df$type), c("numeric", "character", "logical", "date"))
  expect_equal(unname(summary_df$n_missing), c(1L, 1L, 1L, 1L))
  expect_equal(unname(summary_df$n_unique), c(3L, 2L, 2L, 3L))

  numeric_stats <- summary_df$summary_stats[[1]]
  expect_equal(numeric_stats$mean, mean(c(1, 2, 4)))
  expect_equal(numeric_stats$q1, unname(stats::quantile(c(1, 2, 4), 0.25)))
  expect_equal(numeric_stats$median, stats::median(c(1, 2, 4)))
  expect_equal(numeric_stats$q3, unname(stats::quantile(c(1, 2, 4), 0.75)))

  date_stats <- summary_df$summary_stats[[4]]
  expect_equal(date_stats$min, as.Date("2025-01-01"))
  expect_equal(date_stats$max, as.Date("2025-01-04"))
})

test_that("categorical distributions collapse extra levels into Other", {
  df <- data.frame(category = c("a", "b", "c", "d", "a", "b", "e"), stringsAsFactors = FALSE)

  summary_df <- summarize_columns(df, top_n = 2)
  counts <- summary_df$distribution_data[[1]]$counts

  expect_equal(counts$level, c("a", "b", "Other"))
  expect_equal(counts$count, c(2L, 2L, 3L))
  expect_equal(summary_df$distribution_data[[1]]$total, 7L)

  top_levels <- summary_df$summary_stats[[1]]$top_levels
  expect_equal(top_levels$pct, c(2 / 7, 2 / 7, 1 / 7, 1 / 7, 1 / 7))
})

test_that("numeric histogram metadata includes ranges and totals", {
  df <- data.frame(value = c(1, 2, 3, 4, 5))

  summary_df <- summarize_columns(df)
  distribution <- summary_df$distribution_data[[1]]

  expect_identical(distribution$kind, "histogram")
  expect_true(is.data.frame(distribution$ranges))
  expect_equal(nrow(distribution$ranges), length(distribution$bins))
  expect_equal(distribution$total, 5L)
  expect_identical(distribution$value_type, "numeric")
})
