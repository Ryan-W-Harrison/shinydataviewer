test_that("all-missing columns produce stable summary metadata", {
  df <- data.frame(
    all_missing_num = c(NA_real_, NA_real_),
    all_missing_date = as.Date(c(NA, NA), origin = "1970-01-01")
  )

  summary_df <- summarize_columns(df)

  expect_equal(unname(summary_df$n_missing), c(2L, 2L))
  expect_equal(unname(summary_df$pct_missing), c(1, 1))

  numeric_stats <- summary_df$summary_stats[[1]]
  expect_true(all(is.na(unlist(numeric_stats[c(
    "min",
    "q1",
    "median",
    "q3",
    "mean",
    "max",
    "sd"
  )]))))
  expect_identical(summary_df$distribution_data[[1]]$bins, numeric())

  date_stats <- summary_df$summary_stats[[2]]
  expect_true(is.na(date_stats$min))
  expect_true(is.na(date_stats$median))
  expect_true(is.na(date_stats$max))
})

test_that("zero-row and single-column data frames summarize cleanly", {
  df <- data.frame(value = numeric(0))

  summary_df <- summarize_columns(df)

  expect_equal(nrow(summary_df), 1)
  expect_identical(summary_df$var_name[[1]], "value")
  expect_identical(summary_df$n_missing[[1]], 0L)
  expect_identical(summary_df$pct_missing[[1]], 0)
  expect_identical(summary_df$n_unique[[1]], 0L)
})

test_that("factor and logical columns retain expected types and counts", {
  df <- data.frame(
    category = factor(c("low", "high", "low", NA), levels = c("low", "high")),
    flag = c(TRUE, FALSE, TRUE, NA)
  )

  summary_df <- summarize_columns(df, top_n = 3)

  expect_equal(unname(summary_df$type), c("factor", "logical"))

  factor_counts <- summary_df$distribution_data[[1]]$counts
  expect_equal(factor_counts$level, c("low", "high"))
  expect_equal(factor_counts$count, c(2L, 1L))

  factor_levels <- summary_df$summary_stats[[1]]$top_levels
  expect_equal(factor_levels$pct, c(2 / 3, 1 / 3))

  logical_counts <- summary_df$distribution_data[[2]]$counts
  expect_equal(logical_counts$level, c("TRUE", "FALSE"))
  expect_equal(logical_counts$count, c(2L, 1L))
})

test_that("POSIXct columns are summarized as datetimes", {
  timestamps <- as.POSIXct(
    c("2025-01-01 08:00:00", "2025-01-02 10:30:00", NA, "2025-01-03 11:45:00"),
    tz = "UTC"
  )

  summary_df <- summarize_columns(data.frame(timestamp = timestamps))

  expect_identical(summary_df$type[[1]], "datetime")
  datetime_stats <- summary_df$summary_stats[[1]]
  expect_s3_class(datetime_stats$min, "POSIXct")
  expect_equal(datetime_stats$min, timestamps[[1]])
  expect_equal(datetime_stats$max, timestamps[[4]])

  distribution <- summary_df$distribution_data[[1]]
  expect_identical(distribution$value_type, "datetime")
  expect_s3_class(distribution$ranges$left, "POSIXct")
})

test_that("non-finite numeric values are excluded from stats and histograms", {
  df <- data.frame(value = c(1, 2, Inf, -Inf, NaN, NA))

  summary_df <- summarize_columns(df)
  numeric_stats <- summary_df$summary_stats[[1]]

  expect_equal(summary_df$n_missing[[1]], 2L)
  expect_equal(numeric_stats$min, 1)
  expect_equal(numeric_stats$max, 2)
  expect_equal(numeric_stats$mean, 1.5)
  expect_equal(summary_df$distribution_data[[1]]$total, 2L)
})

test_that("unsupported column classes error clearly", {
  df <- data.frame(value = I(list(1:2, 3:4)))

  expect_error(
    summarize_columns(df),
    "Unsupported column types detected"
  )
})
