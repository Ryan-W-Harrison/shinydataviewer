# Data Viewer Module Wrapped in a Card

Data Viewer Module Wrapped in a Card

## Usage

``` r
data_viewer_card_ui(
  id,
  title = NULL,
  full_screen = TRUE,
  sidebar_title = NULL,
  table_controls_position = c("top", "bottom")
)
```

## Arguments

- id:

  Module id.

- title:

  Optional card title.

- full_screen:

  Whether the wrapper card can enter full screen mode.

- sidebar_title:

  Optional title for the variable summary sidebar.

- table_controls_position:

  Where table pagination controls should appear. One of `"top"` or
  `"bottom"`.

## Value

A card containing the module UI.

## Examples

``` r
ui <- bslib::page_fillable(
  theme = bslib::bs_theme(version = 5),
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Context"),
      bslib::card_body("Supporting content")
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
    data = shiny::reactive(mtcars)
  )
}

if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
