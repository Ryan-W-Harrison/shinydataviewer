#' Package Assets for the Data Viewer Module
#'
#' Internal helpers for attaching the module stylesheet and keeping the viewer
#' portable across host applications.
#'
#' @noRd
data_explorer_dependency <- function() {
  htmltools::htmlDependency(
    name = "shinydataviewer",
    version = "0.0.0.9000",
    src = c(file = data_explorer_www_path()),
    stylesheet = "data-explorer.css",
    script = "data-explorer.js"
  )
}

#' @noRd
with_data_explorer_deps <- function(..., root_class = NULL) {
  root_classes <- paste(c("de-root", root_class), collapse = " ")

  htmltools::tagList(
    htmltools::attachDependencies(
      htmltools::tags$div(class = root_classes, ...),
      data_explorer_dependency()
    )
  )
}

#' @noRd
data_explorer_www_path <- function() {
  installed_path <- app_sys("app/www")

  if (nzchar(installed_path) && dir.exists(installed_path)) {
    return(installed_path)
  }

  for (base_path in candidate_package_roots()) {
    dev_path <- normalizePath(
      file.path(base_path, "inst", "app", "www"),
      winslash = "/",
      mustWork = FALSE
    )

    if (dir.exists(dev_path)) {
      return(dev_path)
    }
  }

  stop("Could not locate data viewer assets in inst/app/www.")
}

candidate_package_roots <- function() {
  paths <- c(getwd(), parent_directories(getwd()))
  unique(normalizePath(paths, winslash = "/", mustWork = FALSE))
}

parent_directories <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  parents <- character()

  repeat {
    next_path <- dirname(path)

    if (identical(next_path, path)) {
      break
    }

    parents <- c(parents, next_path)
    path <- next_path
  }

  parents
}
