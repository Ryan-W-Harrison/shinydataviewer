# Data Viewer Module Server

Data Viewer Module Server

## Usage

``` r
data_viewer_server(
  id,
  data,
  top_n = 6,
  default_page_size = NULL,
  page_size_options = c(15, 25, 50, 100),
  searchable = TRUE,
  filterable = TRUE,
  sortable = TRUE,
  summary_card_fn = variable_summary_card,
  reactable_theme = NULL,
  default_col_def = NULL,
  reactable_args = list()
)
```

## Arguments

- id:

  Module id.

- data:

  Reactive returning a data frame.

- top_n:

  Maximum number of categorical levels to keep in compact summary views
  before collapsing the remainder into `"Other"`.

- default_page_size:

  Optional default number of rows to show in the table. If `NULL`, the
  module shows all rows up to the largest page-size option.

- page_size_options:

  Page-size options shown in the table controls.

- searchable:

  Whether the table search box is enabled.

- filterable:

  Whether column filters are enabled.

- sortable:

  Whether column sorting is enabled.

- summary_card_fn:

  Function used to render each variable summary card. It must accept
  `(summary_row, index)` and return a Shiny UI tag.

- reactable_theme:

  Optional
  [`reactable::reactableTheme()`](https://glin.github.io/reactable/reference/reactableTheme.html)
  object. If `NULL`, the module uses its built-in theme.

- default_col_def:

  Optional default
  [`reactable::colDef()`](https://glin.github.io/reactable/reference/colDef.html)
  to apply to all columns. If `NULL`, the module uses its built-in
  default column definition.

- reactable_args:

  Optional named list of additional arguments passed to
  [`reactable::reactable()`](https://glin.github.io/reactable/reference/reactable.html).
  These values override the module defaults.

## Value

Invisibly returns a list of reactives.

## Examples

``` r
custom_summary_card <- function(summary_row, index) {
  htmltools::tags$div(
    class = "custom-summary-card",
    sprintf("%s: %s", summary_row$var_name[[1]], summary_row$type[[1]])
  )
}

ui <- bslib::page_fillable(
  theme = bslib::bs_theme(version = 5),
  data_viewer_card_ui("viewer", title = "Iris")
)

server <- function(input, output, session) {
  data_viewer_server(
    "viewer",
    data = shiny::reactive(iris),
    searchable = TRUE,
    filterable = TRUE,
    sortable = TRUE,
    summary_card_fn = custom_summary_card
  )
}

if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
