library(shiny)
library(bslib)
library(shinydataviewer)

ui <- page_sidebar(
  title = "Upload a dataset",
  theme = bs_theme(version = 5),
  fillable = TRUE,
  sidebar = sidebar(
    fileInput(
      "dataset_file",
      "Choose a CSV or RDS file",
      accept = c("text/csv", ".csv", ".rds")
    ),
    checkboxInput("header", "CSV has a header row", value = TRUE),
    selectInput(
      "separator",
      "CSV separator",
      choices = c(
        "Comma" = ",",
        "Semicolon" = ";",
        "Tab" = "\t"
      )
    ),
    helpText(
      "Upload a CSV file or an RDS file containing a data frame.",
      "Only upload RDS files from sources you trust."
    )
  ),
  data_viewer_card_ui(
    "viewer",
    title = "Dataset",
    sidebar_title = "Variables"
  )
)

server <- function(input, output, session) {
  uploaded_data <- reactive({
    req(input$dataset_file)
    extension <- tolower(tools::file_ext(input$dataset_file$name))

    tryCatch(
      switch(
        extension,
        csv = read.csv(
          input$dataset_file$datapath,
          header = input$header,
          sep = input$separator,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        rds = readRDS(input$dataset_file$datapath),
        validate(need(FALSE, "Please upload a .csv or .rds file."))
      ),
      error = function(error) {
        validate(need(
          FALSE,
          paste("Could not read the uploaded file:", conditionMessage(error))
        ))
      }
    )
  })

  data_viewer_server(
    "viewer",
    data = uploaded_data
  )
}

shinyApp(ui, server)
