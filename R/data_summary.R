#' Summarize Columns in a Data Frame
#'
#' @param df A data frame to summarize.
#' @param top_n Maximum number of categorical levels to keep before collapsing
#'   the remainder into `"Other"`.
#'
#' @return A data frame with one row per column.
#' @export
summarize_columns <- function(df, top_n = 6) {
  stopifnot(is.data.frame(df))

  data.frame(
    var_name = names(df),
    type = vapply(df, column_type, character(1)),
    n_missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    pct_missing = vapply(df, pct_missing, numeric(1)),
    n_unique = vapply(df, unique_non_missing, integer(1)),
    summary_stats = I(lapply(df, summarize_vector)),
    distribution_data = I(lapply(df, function(x) {
      distribution_data(x, top_n = top_n)
    })),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

pct_missing <- function(x) {
  if (length(x) == 0) {
    return(0)
  }

  mean(is.na(x))
}

column_type <- function(x) {
  if (inherits(x, "Date")) {
    return("date")
  }

  if (is.numeric(x)) {
    return("numeric")
  }

  if (is.factor(x)) {
    return("factor")
  }

  if (is.logical(x)) {
    return("logical")
  }

  if (is.character(x)) {
    return("character")
  }

  class(x)[1]
}

unique_non_missing <- function(x) {
  sum(!is.na(unique(x)))
}

summarize_vector <- function(x) {
  type <- column_type(x)
  n_missing <- sum(is.na(x))

  if (type == "numeric") {
    values <- x[!is.na(x)]
    quartiles <- safe_quantiles(values)

    return(list(
      missing = n_missing,
      min = safe_numeric_stat(values, min),
      q1 = quartiles[[1]],
      median = safe_numeric_stat(values, stats::median),
      q3 = quartiles[[2]],
      max = safe_numeric_stat(values, max),
      mean = safe_numeric_stat(values, mean),
      sd = safe_numeric_stat(values, stats::sd)
    ))
  }

  if (type == "date") {
    values <- x[!is.na(x)]
    median_date <- if (length(values) > 0) {
      as.Date(stats::median(as.numeric(values)), origin = "1970-01-01")
    } else {
      as.Date(NA)
    }

    return(list(
      missing = n_missing,
      min = safe_date_stat(values, min),
      median = median_date,
      max = safe_date_stat(values, max)
    ))
  }

  counts <- categorical_counts(x)

  list(
    missing = n_missing,
    n_unique = unique_non_missing(x),
    top_levels = utils::head(
      transform(
        counts,
        pct = safe_share(count, sum(count))
      ),
      6
    )
  )
}

distribution_data <- function(x, top_n = 6) {
  type <- column_type(x)

  if (type %in% c("numeric", "date")) {
    values <- x[!is.na(x)]

    if (length(values) == 0) {
      return(list(kind = "histogram", bins = numeric(), ranges = NULL))
    }

    hist_values <- if (type == "date") as.numeric(values) else values
    hist_info <- graphics::hist(hist_values, plot = FALSE, breaks = "Sturges")
    ranges <- data.frame(
      left = utils::head(hist_info$breaks, -1),
      right = utils::tail(hist_info$breaks, -1)
    )

    if (type == "date") {
      ranges$left <- as.Date(ranges$left, origin = "1970-01-01")
      ranges$right <- as.Date(ranges$right, origin = "1970-01-01")
    }

    return(list(
      kind = "histogram",
      bins = unname(hist_info$counts),
      ranges = ranges,
      total = length(values),
      value_type = type
    ))
  }

  counts <- categorical_counts(x)

  if (nrow(counts) > top_n) {
    top_counts <- counts[seq_len(top_n), , drop = FALSE]
    other_count <- sum(counts$count[-seq_len(top_n)])
    counts <- rbind(
      top_counts,
      data.frame(level = "Other", count = other_count, stringsAsFactors = FALSE)
    )
  }

  list(
    kind = "categorical",
    counts = counts,
    total = sum(counts$count)
  )
}

categorical_counts <- function(x) {
  values <- x[!is.na(x)]

  if (length(values) == 0) {
    return(data.frame(
      level = character(0),
      count = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  values_chr <- as.character(values)
  counts <- sort(table(values_chr), decreasing = TRUE)

  data.frame(
    level = names(counts),
    count = as.integer(counts),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

safe_numeric_stat <- function(x, fn) {
  if (length(x) == 0) {
    return(NA_real_)
  }

  fn(x)
}

safe_quantiles <- function(x) {
  if (length(x) == 0) {
    return(c(NA_real_, NA_real_))
  }

  stats::quantile(x, probs = c(0.25, 0.75), names = FALSE, na.rm = TRUE)
}

safe_date_stat <- function(x, fn) {
  if (length(x) == 0) {
    return(as.Date(NA))
  }

  fn(x)
}
