# Summarize Columns in a Data Frame

Summarize Columns in a Data Frame

## Usage

``` r
summarize_columns(df, top_n = 6)
```

## Arguments

- df:

  A data frame to summarize.

- top_n:

  Maximum number of categorical levels to keep before collapsing the
  remainder into `"Other"`.

## Value

A data frame with one row per column.
