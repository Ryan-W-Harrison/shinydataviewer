#' Summarize Columns in a Data Frame
#'
#' @param df A data frame to summarize. Supported column classes are numeric,
#'   integer, character, factor, logical, `Date`, `POSIXct`/`POSIXt`, and
#'   `hms`/`difftime`.
#' @param top_n Maximum number of categorical levels to keep before collapsing
#'   the remainder into `"Other"`.
#'
#' @return A data frame with one row per column and the following columns:
#'   `var_name`, `type`, `n_missing`, `pct_missing`, `n_unique`,
#'   `summary_stats`, and `distribution_data`. `summary_stats` is a list-column
#'   containing per-type summary values used by the details accordion.
#'   `distribution_data` is a list-column containing precomputed histogram or
#'   categorical count payloads used by the compact mini charts.
#' @export
summarize_columns <- function(df, top_n = 6) {
  stopifnot(is.data.frame(df))
  validate_top_n(top_n)
  validate_supported_columns(df)

  data.frame(
    var_name = names(df),
    type = vapply(df, column_type, character(1)),
    n_missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    pct_missing = vapply(df, pct_missing, numeric(1)),
    n_unique = vapply(df, unique_non_missing, integer(1)),
    summary_stats = I(lapply(df, function(x) {
      summarize_vector(x, top_n = top_n)
    })),
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
  if (inherits(x, "difftime")) {
    return("time")
  }

  if (inherits(x, "POSIXt")) {
    return("datetime")
  }

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

summarize_vector <- function(x, top_n = 6) {
  type <- column_type(x)
  n_missing <- sum(is.na(x))

  if (type == "numeric") {
    values <- finite_numeric_values(x)
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

  if (type == "datetime") {
    values <- finite_datetime_values(x)
    median_datetime <- if (length(values) > 0) {
      as.POSIXct(
        stats::median(as.numeric(values)),
        origin = "1970-01-01",
        tz = datetime_tz(x)
      )
    } else {
      as.POSIXct(NA, origin = "1970-01-01", tz = datetime_tz(x))
    }

    return(list(
      missing = n_missing,
      min = safe_datetime_stat(values, min, tz = datetime_tz(x)),
      median = median_datetime,
      max = safe_datetime_stat(values, max, tz = datetime_tz(x))
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

  if (type == "time") {
    values <- finite_time_values(x)
    seconds <- as.numeric(values, units = "secs")

    return(list(
      missing = n_missing,
      min = seconds_as_difftime(safe_numeric_stat(seconds, min)),
      median = seconds_as_difftime(safe_numeric_stat(seconds, stats::median)),
      max = seconds_as_difftime(safe_numeric_stat(seconds, max))
    ))
  }

  counts <- categorical_counts(x)
  top_levels <- if (nrow(counts) == 0) {
    data.frame(
      level = character(),
      count = integer(),
      pct = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    utils::head(
      transform(
        counts,
        pct = safe_share(count, sum(count))
      ),
      top_n
    )
  }

  list(
    missing = n_missing,
    n_unique = unique_non_missing(x),
    top_levels = top_levels
  )
}

distribution_data <- function(x, top_n = 6) {
  type <- column_type(x)

  if (type %in% c("numeric", "date", "datetime", "time")) {
    values <- summarized_distribution_values(x, type)

    if (length(values) == 0) {
      return(list(kind = "histogram", bins = numeric(), ranges = NULL))
    }

    hist_values <- if (type %in% c("date", "datetime")) {
      as.double(as.numeric(values))
    } else if (type == "time") {
      as.numeric(values, units = "secs")
    } else {
      values
    }
    hist_origin <- if (type %in% c("date", "datetime")) min(hist_values) else 0
    hist_input <- if (type %in% c("date", "datetime")) {
      hist_values - hist_origin
    } else {
      hist_values
    }
    hist_info <- graphics::hist(hist_input, plot = FALSE, breaks = "Sturges")
    ranges <- data.frame(
      left = utils::head(hist_info$breaks, -1) + hist_origin,
      right = utils::tail(hist_info$breaks, -1) + hist_origin
    )

    if (type == "date") {
      ranges$left <- as.Date(ranges$left, origin = "1970-01-01")
      ranges$right <- as.Date(ranges$right, origin = "1970-01-01")
    }

    if (type == "datetime") {
      tz <- datetime_tz(x)
      ranges$left <- as.POSIXct(ranges$left, origin = "1970-01-01", tz = tz)
      ranges$right <- as.POSIXct(ranges$right, origin = "1970-01-01", tz = tz)
    }

    if (type == "time") {
      ranges$left <- seconds_as_difftime(ranges$left)
      ranges$right <- seconds_as_difftime(ranges$right)
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

safe_datetime_stat <- function(x, fn, tz = "UTC") {
  if (length(x) == 0) {
    return(as.POSIXct(NA, origin = "1970-01-01", tz = tz))
  }

  fn(x)
}

finite_numeric_values <- function(x) {
  values <- x[!is.na(x)]
  values[is.finite(values)]
}

finite_datetime_values <- function(x) {
  values <- x[!is.na(x)]
  numeric_values <- as.numeric(values)
  values[is.finite(numeric_values)]
}

finite_time_values <- function(x) {
  values <- x[!is.na(x)]
  numeric_values <- as.numeric(values, units = "secs")
  values[is.finite(numeric_values)]
}

seconds_as_difftime <- function(x) {
  as.difftime(x, units = "secs")
}

summarized_distribution_values <- function(x, type) {
  if (identical(type, "numeric")) {
    return(finite_numeric_values(x))
  }

  if (identical(type, "datetime")) {
    return(finite_datetime_values(x))
  }

  if (identical(type, "time")) {
    return(finite_time_values(x))
  }

  x[!is.na(x)]
}

datetime_tz <- function(x) {
  tz <- attr(x, "tzone")

  if (length(tz) == 0 || !nzchar(tz[[1]])) {
    return("UTC")
  }

  tz[[1]]
}

validate_top_n <- function(top_n) {
  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n)) {
    stop("`top_n` must be a single positive integer.", call. = FALSE)
  }

  if (top_n < 1 || !isTRUE(all.equal(top_n, round(top_n)))) {
    stop("`top_n` must be a single positive integer.", call. = FALSE)
  }

  invisible(as.integer(top_n))
}

validate_supported_columns <- function(df) {
  unsupported <- names(df)[!vapply(df, is_supported_column, logical(1))]

  if (length(unsupported) == 0) {
    return(invisible(df))
  }

  unsupported_labels <- vapply(
    unsupported,
    function(name) {
      sprintf(
        "%s <%s>",
        name,
        paste(class(df[[name]]), collapse = "/")
      )
    },
    character(1)
  )

  stop(
    sprintf(
      paste(
        "Unsupported column types detected.",
        "Supported types are numeric, integer, character, factor, logical, Date, POSIXct/POSIXt, and hms/difftime.",
        "Problem columns: %s"
      ),
      paste(unsupported_labels, collapse = ", ")
    ),
    call. = FALSE
  )
}

is_supported_column <- function(x) {
  is.factor(x) ||
    is.character(x) ||
    is.logical(x) ||
    is.numeric(x) ||
    inherits(x, "Date") ||
    inherits(x, "POSIXt") ||
    inherits(x, "difftime")
}
