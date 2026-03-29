# Get Started

``` r
library(shinydataviewer)
library(shiny)
library(bslib)
#> 
#> Attaching package: 'bslib'
#> The following object is masked from 'package:utils':
#> 
#>     page
```

`shinydataviewer` provides a reusable Shiny module for viewing data
with:

- a `reactable` data viewer
- a variable summary sidebar
- Bootstrap 5 compatible styling via `bslib`

## Minimal module

Use
[`data_viewer_ui()`](https://ryan-w-harrison.github.io/shinydataviewer/reference/data_viewer_ui.md)
when the viewer should manage its own table card.

``` r
ui <- page_fillable(
  theme = bs_theme(version = 5),
  data_viewer_ui("viewer")
)

server <- function(input, output, session) {
  data_viewer_server(
    "viewer",
    data = reactive(iris)
  )
}

shinyApp(ui, server)
```

## Embedded card

Use
[`data_viewer_card_ui()`](https://ryan-w-harrison.github.io/shinydataviewer/reference/data_viewer_card_ui.md)
when the viewer belongs inside another layout.

``` r
ui <- page_fillable(
  theme = bs_theme(version = 5),
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Context"),
      card_body("Supporting content")
    ),
    data_viewer_card_ui(
      "viewer",
      title = "Dataset",
      full_screen = FALSE
    )
  )
)

server <- function(input, output, session) {
  data_viewer_server(
    "viewer",
    data = reactive(mtcars)
  )
}

shinyApp(ui, server)
```

## Summary helper

The variable sidebar is backed by
[`summarize_columns()`](https://ryan-w-harrison.github.io/shinydataviewer/reference/summarize_columns.md).

``` r
summary_df <- summarize_columns(head(iris), top_n = 4)

summary_df[c("var_name", "type", "n_missing", "n_unique")]
#>                  var_name    type n_missing n_unique
#> Sepal.Length Sepal.Length numeric         0        6
#> Sepal.Width   Sepal.Width numeric         0        6
#> Petal.Length Petal.Length numeric         0        4
#> Petal.Width   Petal.Width numeric         0        2
#> Species           Species  factor         0        1
```

## Example app

A runnable embedded example is included at
`inst/examples/embedded-card-example.R`.
