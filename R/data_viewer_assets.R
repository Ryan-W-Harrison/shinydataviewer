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
  for (base_path in candidate_package_roots()) {
    if (!is_shinydataviewer_root(base_path)) {
      next
    }

    dev_path <- normalizePath(
      file.path(base_path, "inst", "app", "www"),
      winslash = "/",
      mustWork = FALSE
    )

    if (dir.exists(dev_path)) {
      return(dev_path)
    }
  }

  installed_path <- app_sys("app/www")

  if (nzchar(installed_path) && dir.exists(installed_path)) {
    return(installed_path)
  }

  stop("Could not locate data viewer assets in inst/app/www.")
}

data_viewer_dependency_version <- function() {
  for (base_path in candidate_package_roots()) {
    if (!is_shinydataviewer_root(base_path)) {
      next
    }

    description_path <- file.path(base_path, "DESCRIPTION")

    if (!file.exists(description_path)) {
      next
    }

    local_version <- tryCatch(
      {
        description <- read.dcf(description_path)
        description[1, "Version"]
      },
      error = function(...) NULL
    )

    if (!is.null(local_version) && nzchar(local_version)) {
      return(local_version)
    }
  }

  installed_version <- tryCatch(
    as.character(utils::packageVersion("shinydataviewer")),
    error = function(...) NULL
  )

  if (!is.null(installed_version)) {
    return(installed_version)
  }

  stop("Could not determine the shinydataviewer package version.")
}

is_shinydataviewer_root <- function(path) {
  description_path <- file.path(path, "DESCRIPTION")

  if (!file.exists(description_path)) {
    return(FALSE)
  }

  tryCatch(
    {
      description <- read.dcf(description_path)
      identical(unname(description[1, "Package"]), "shinydataviewer")
    },
    error = function(...) FALSE
  )
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
