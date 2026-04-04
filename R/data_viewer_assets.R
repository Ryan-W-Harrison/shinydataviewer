#' Package Assets for the Data Viewer Module
#'
#' Internal helpers for attaching the module stylesheet and keeping the viewer
#' portable across host applications.
#'
#' @noRd
data_viewer_dependency <- function() {
  htmltools::htmlDependency(
    name = "shinydataviewer",
    version = data_viewer_dependency_version(),
    src = c(file = data_viewer_www_path()),
    stylesheet = "data-viewer.css",
    script = "data-viewer.js"
  )
}

#' @noRd
with_data_viewer_deps <- function(..., root_class = NULL) {
  root_classes <- paste(c("de-root", root_class), collapse = " ")

  htmltools::tagList(
    htmltools::attachDependencies(
      htmltools::tags$div(class = root_classes, ...),
      data_viewer_dependency()
    )
  )
}

#' @noRd
data_viewer_www_path <- function() {
  installed_path <- app_sys("app/www")

  if (nzchar(installed_path) && dir.exists(installed_path)) {
    return(installed_path)
  }

  stop(
    paste(
      "Could not locate installed data viewer assets.",
      "Reinstall the package so `inst/app/www` is available through `system.file()`."
    ),
    call. = FALSE
  )
}

data_viewer_dependency_version <- function() {
  installed_version <- tryCatch(
    as.character(utils::packageVersion("shinydataviewer")),
    error = function(...) NULL
  )

  if (!is.null(installed_version)) {
    return(installed_version)
  }

  stop("Could not determine the shinydataviewer package version.")
}
