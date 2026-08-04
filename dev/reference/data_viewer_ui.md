# Data Viewer Module UI

Data Viewer Module UI

## Usage

``` r
data_viewer_ui(
  id,
  standalone = TRUE,
  table_title = NULL,
  sidebar_title = NULL,
  table_controls_position = c("top", "bottom"),
  plot_color = NULL
)
```

## Arguments

- id:

  Module id.

- standalone:

  Whether to render with the built-in table card. Set to `FALSE` when
  embedding inside another
  [`bslib::card()`](https://rstudio.github.io/bslib/reference/card.html).

- table_title:

  Optional title for the table region.

- sidebar_title:

  Optional title for the variable summary sidebar.

- table_controls_position:

  Where table pagination controls should appear. One of `"top"` or
  `"bottom"`.

- plot_color:

  Optional CSS color used by the variable-summary mini plots. Accepts
  values such as `"#2c7fb8"`, `"rebeccapurple"`, `"rgb(44 127 184)"`, or
  `"var(--bs-success)"`. If `NULL`, the active Bootstrap primary color
  is used.

## Value

The module UI.

## Examples

``` r
ui <- bslib::page_fillable(
  theme = bslib::bs_theme(version = 5),
  data_viewer_ui("viewer")
)

server <- function(input, output, session) {
  data_viewer_server(
    "viewer",
    data = shiny::reactive(iris)
  )
}

if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
