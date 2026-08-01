# Summarize Columns in a Data Frame

Summarize Columns in a Data Frame

## Usage

``` r
summarize_columns(df, top_n = 6)
```

## Arguments

- df:

  A data frame to summarize. Supported column classes are numeric,
  integer, character, factor, logical, `Date`, `POSIXct`/`POSIXt`, and
  `hms`/`difftime`.

- top_n:

  Maximum number of categorical levels to keep before collapsing the
  remainder into `"Other"`.

## Value

A data frame with one row per column and the following columns:
`var_name`, `type`, `n_missing`, `pct_missing`, `n_unique`,
`summary_stats`, and `distribution_data`. `summary_stats` is a
list-column containing per-type summary values used by the details
accordion. `distribution_data` is a list-column containing precomputed
histogram or categorical count payloads used by the compact mini charts.

## Examples

``` r
column_summary <- summarize_columns(iris)
column_summary[c(
  "var_name",
  "type",
  "n_missing",
  "pct_missing",
  "n_unique"
)]
#>                  var_name    type n_missing pct_missing n_unique
#> Sepal.Length Sepal.Length numeric         0           0       35
#> Sepal.Width   Sepal.Width numeric         0           0       23
#> Petal.Length Petal.Length numeric         0           0       43
#> Petal.Width   Petal.Width numeric         0           0       22
#> Species           Species  factor         0           0        3
```
