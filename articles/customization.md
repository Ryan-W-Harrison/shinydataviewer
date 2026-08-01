# Customization

``` r

library(shinydataviewer)
library(shiny)
library(bslib)
#> 
#> Attaching package: 'bslib'
#> The following object is masked from 'package:utils':
#> 
#>     page
library(reactable)
```

The module exposes a small set of customization hooks intended to
balance drop-in defaults with downstream flexibility.

## Table behavior

[`data_viewer_server()`](https://ryan-w-harrison.github.io/shinydataviewer/reference/data_viewer_server.md)
exposes first-class arguments for common table controls:

- `searchable`
- `filterable`
- `sortable`
- `default_page_size`
- `page_size_options`

For lower-level
[`reactable()`](https://glin.github.io/reactable/reference/reactable.html)
overrides, use `reactable_args`.

`default_page_size` must be one of the values supplied in
`page_size_options`, and `page_size_options` must contain positive
integers.

``` r

data_viewer_server(
  "viewer",
  data = reactive(mtcars),
  searchable = TRUE,
  filterable = FALSE,
  sortable = TRUE,
  default_page_size = 25,
  page_size_options = c(25, 50, 100),
  reactable_args = list(
    showPageInfo = FALSE
  )
)
```

## Table controls position

The UI helpers support top or bottom table controls:

``` r

data_viewer_card_ui(
  "viewer",
  table_controls_position = "bottom"
)
```

## Custom summary cards

Use `summary_card_fn` to replace the default variable card renderer
while keeping the module’s summary computation and table logic.

The custom function must accept at least `summary_row` and `index`,
where `summary_row` is a one-row data frame produced by
[`summarize_columns()`](https://ryan-w-harrison.github.io/shinydataviewer/reference/summarize_columns.md).

``` r

custom_summary_card <- function(summary_row, index) {
  htmltools::tags$div(
    class = "custom-summary-card",
    htmltools::tags$strong(summary_row$var_name[[1]]),
    htmltools::tags$span(sprintf(" (%s)", summary_row$type[[1]]))
  )
}

data_viewer_server(
  "viewer",
  data = reactive(iris),
  summary_card_fn = custom_summary_card
)
```

A typical use case is treating identifier-like columns differently from
ordinary categorical fields, for example by suppressing the mini chart
and showing a “High cardinality” badge instead.

The module validates unsupported column classes early, so custom
summary-card logic can assume the incoming `summary_row` corresponds to
one of the supported viewer types.

## Theme integration

The module styles use Bootstrap variables, so they follow the active
`bslib` theme and `brand.yml` values automatically.

They also respond to live light and dark mode changes from
[`bslib::input_dark_mode()`](https://rstudio.github.io/bslib/reference/input_dark_mode.html)
and
[`bslib::toggle_dark_mode()`](https://rstudio.github.io/bslib/reference/input_dark_mode.html).

``` r

ui <- page_fillable(
  theme = bs_theme(version = 5, brand = "brand.yml"),
  data_viewer_card_ui("viewer")
)
```
