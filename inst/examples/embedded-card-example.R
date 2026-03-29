library(shiny)
library(bslib)
library(shinydataviewer)

sample_data <- data.frame(
  model = rownames(datasets::mtcars),
  datasets::mtcars,
  row.names = NULL,
  check.names = FALSE
)
sample_data$cyl <- factor(sample_data$cyl)
sample_data$am <- factor(sample_data$am, labels = c("Automatic", "Manual"))

ui <- page_fillable(
  theme = bs_theme(version = 5),
  fillable = TRUE,
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Context"),
      card_body(
        "The explorer below is embedded inside a larger host card."
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Customer Data"),
      card_body(
        fill = TRUE,
        data_viewer_card_ui(
          "explorer",
          title = NULL,
          full_screen = FALSE,
          sidebar_title = NULL
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data_viewer_server("explorer", reactive(sample_data))
}

shinyApp(ui, server)
