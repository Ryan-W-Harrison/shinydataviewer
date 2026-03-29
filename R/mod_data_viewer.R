#' Data Viewer Module UI
#'
#' @param id Module id.
#' @param standalone Whether to render with the built-in table card. Set to
#'   `FALSE` when embedding inside another `bslib::card()`.
#' @param table_title Optional title for the table region.
#' @param sidebar_title Optional title for the variable summary sidebar.
#' @param table_controls_position Where table pagination controls should appear.
#'   One of `"top"` or `"bottom"`.
#'
#' @return The module UI.
#'
#' @examples
#' ui <- bslib::page_fillable(
#'   theme = bslib::bs_theme(version = 5),
#'   data_viewer_ui("viewer")
#' )
#'
#' server <- function(input, output, session) {
#'   data_viewer_server(
#'     "viewer",
#'     data = shiny::reactive(iris)
#'   )
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
#' @export
data_viewer_ui <- function(
  id,
  standalone = TRUE,
  table_title = NULL,
  sidebar_title = NULL,
  table_controls_position = c("top", "bottom")
) {
  table_controls_position <- match.arg(table_controls_position)

  with_data_viewer_deps(
    data_viewer_layout_ui(
      id,
      standalone = standalone,
      table_title = table_title,
      sidebar_title = sidebar_title
    ),
    root_class = paste0("de-root--controls-", table_controls_position)
  )
}

data_viewer_layout_ui <- function(
  id,
  standalone = TRUE,
  table_title = NULL,
  sidebar_title = NULL
) {
  ns <- shiny::NS(id)
  table_region <- if (standalone) {
    card_header <- if (is.null(table_title)) {
      NULL
    } else {
      bslib::card_header(table_title)
    }

    bslib::card(
      full_screen = TRUE,
      class = "de-table-card",
      card_header,
      bslib::card_body(
        class = "de-table-card__body",
        reactable::reactableOutput(ns("data_table"), height = "100%")
      )
    )
  } else {
    htmltools::tags$div(
      class = "de-table-region",
      reactable::reactableOutput(ns("data_table"), height = "100%")
    )
  }

  bslib::layout_sidebar(
    fillable = TRUE,
    gap = "1rem",
    sidebar = bslib::sidebar(
      id = ns("summary_sidebar"),
      title = sidebar_title,
      width = "360px",
      open = "always",
      class = "de-sidebar",
      htmltools::tags$div(
        class = "de-sidebar__inner",
        shiny::uiOutput(ns("summary_panel"), container = htmltools::div)
      )
    ),
    table_region
  )
}

#' Data Viewer Module Wrapped in a Card
#'
#' @param id Module id.
#' @param title Optional card title.
#' @param full_screen Whether the wrapper card can enter full screen mode.
#' @param sidebar_title Optional title for the variable summary sidebar.
#' @param table_controls_position Where table pagination controls should appear.
#'   One of `"top"` or `"bottom"`.
#'
#' @return A card containing the module UI.
#' @examples
#' ui <- bslib::page_fillable(
#'   theme = bslib::bs_theme(version = 5),
#'   bslib::layout_columns(
#'     col_widths = c(4, 8),
#'     bslib::card(
#'       bslib::card_header("Context"),
#'       bslib::card_body("Supporting content")
#'     ),
#'     data_viewer_card_ui(
#'       "viewer",
#'       title = "Dataset",
#'       full_screen = FALSE
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data_viewer_server(
#'     "viewer",
#'     data = shiny::reactive(mtcars)
#'   )
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
#' @export
data_viewer_card_ui <- function(
  id,
  title = NULL,
  full_screen = TRUE,
  sidebar_title = NULL,
  table_controls_position = c("top", "bottom")
) {
  table_controls_position <- match.arg(table_controls_position)
  card_header <- if (is.null(title)) NULL else bslib::card_header(title)

  with_data_viewer_deps(
    bslib::card(
      class = "de-module-card",
      full_screen = full_screen,
      fill = TRUE,
      card_header,
      bslib::card_body(
        class = "de-module-card__body",
        fill = TRUE,
        data_viewer_layout_ui(
          id,
          standalone = FALSE,
          sidebar_title = sidebar_title
        )
      )
    ),
    root_class = paste0("de-root--controls-", table_controls_position)
  )
}

#' Data Viewer Module Server
#'
#' @param id Module id.
#' @param data Reactive returning a data frame.
#' @param top_n Maximum number of categorical levels to keep in compact summary
#'   views before collapsing the remainder into `"Other"`.
#' @param default_page_size Optional default number of rows to show in the table.
#'   If `NULL`, the module shows all rows up to the largest page-size option.
#' @param page_size_options Page-size options shown in the table controls.
#' @param searchable Whether the table search box is enabled.
#' @param filterable Whether column filters are enabled.
#' @param sortable Whether column sorting is enabled.
#' @param summary_card_fn Function used to render each variable summary card.
#'   It must accept `(summary_row, index)` and return a Shiny UI tag.
#' @param reactable_theme Optional `reactable::reactableTheme()` object. If
#'   `NULL`, the module uses its built-in theme.
#' @param default_col_def Optional default `reactable::colDef()` to apply to all
#'   columns. If `NULL`, the module uses its built-in default column definition.
#' @param reactable_args Optional named list of additional arguments passed to
#'   `reactable::reactable()`. These values override the module defaults.
#'
#' @return Invisibly returns a list of reactives.
#'
#' @examples
#' custom_summary_card <- function(summary_row, index) {
#'   htmltools::tags$div(
#'     class = "custom-summary-card",
#'     sprintf("%s: %s", summary_row$var_name[[1]], summary_row$type[[1]])
#'   )
#' }
#'
#' ui <- bslib::page_fillable(
#'   theme = bslib::bs_theme(version = 5),
#'   data_viewer_card_ui("viewer", title = "Iris")
#' )
#'
#' server <- function(input, output, session) {
#'   data_viewer_server(
#'     "viewer",
#'     data = shiny::reactive(iris),
#'     searchable = TRUE,
#'     filterable = TRUE,
#'     sortable = TRUE,
#'     summary_card_fn = custom_summary_card
#'   )
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
#' @export
data_viewer_server <- function(
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
) {
  shiny::moduleServer(id, function(input, output, session) {
    validate_summary_card_fn(summary_card_fn)

    dataset <- shiny::reactive({
      df <- data()

      validate_data_frame(df)
      as.data.frame(df, stringsAsFactors = FALSE)
    })

    column_summary <- shiny::reactive({
      summarize_columns(dataset(), top_n = top_n)
    })

    output$summary_panel <- shiny::renderUI({
      summary_df <- column_summary()

      if (nrow(summary_df) == 0) {
        return(
          bslib::card(
            class = "de-empty-card",
            bslib::card_body("No columns available.")
          )
        )
      }

      htmltools::tagList(
        lapply(
          seq_len(nrow(summary_df)),
          function(i) summary_card_fn(summary_df[i, , drop = FALSE], i)
        )
      )
    })

    output$data_table <- reactable::renderReactable({
      df <- dataset()
      resolved_page_size <- resolve_default_page_size(
        n_rows = nrow(df),
        default_page_size = default_page_size,
        page_size_options = page_size_options
      )
      table_theme <- reactable_theme %||% default_reactable_theme()
      col_def <- default_col_def %||%
        reactable::colDef(
          minWidth = 120,
          headerClass = "de-table-header"
        )
      table_args <- utils::modifyList(
        list(
          data = df,
          searchable = searchable,
          filterable = filterable,
          sortable = sortable,
          pagination = TRUE,
          bordered = FALSE,
          striped = TRUE,
          highlight = TRUE,
          resizable = TRUE,
          compact = TRUE,
          wrap = FALSE,
          defaultPageSize = resolved_page_size,
          showPageSizeOptions = TRUE,
          showPageInfo = TRUE,
          pageSizeOptions = page_size_options,
          minRows = 1,
          defaultColDef = col_def,
          theme = table_theme
        ),
        reactable_args
      )

      do.call(reactable::reactable, table_args)
    })

    invisible(
      list(
        data = dataset,
        summary = column_summary
      )
    )
  })
}

validate_summary_card_fn <- function(x) {
  if (!is.function(x)) {
    stop("`summary_card_fn` must be a function.", call. = FALSE)
  }

  invisible(x)
}

validate_data_frame <- function(x) {
  if (is.null(x)) {
    shiny::validate(shiny::need(FALSE, "Data is not available."))
  }

  if (!is.data.frame(x)) {
    shiny::validate(shiny::need(FALSE, "`data` must be a data.frame."))
  }

  if (ncol(x) == 0) {
    shiny::validate(shiny::need(
      FALSE,
      "Dataset must contain at least one column."
    ))
  }
}

resolve_default_page_size <- function(
  n_rows,
  default_page_size = NULL,
  page_size_options = c(15, 25, 50, 100)
) {
  if (!is.null(default_page_size)) {
    return(max(1L, as.integer(default_page_size[[1]])))
  }

  max_default <- max(page_size_options, na.rm = TRUE)
  min(max(1L, n_rows), max_default)
}

default_reactable_theme <- function() {
  reactable::reactableTheme(
    borderColor = "var(--bs-border-color)",
    stripedColor = "color-mix(in srgb, var(--bs-body-bg) 88%, white 12%)",
    highlightColor = "rgba(var(--bs-primary-rgb), 0.08)",
    rowSelectedStyle = list(
      backgroundColor = "rgba(var(--bs-primary-rgb), 0.14)"
    ),
    searchInputStyle = list(width = "100%"),
    cellPadding = "0.65rem 0.75rem",
    style = list(
      fontSize = "0.95rem",
      color = "var(--bs-body-color)",
      backgroundColor = "var(--bs-body-bg)"
    )
  )
}
