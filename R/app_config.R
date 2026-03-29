#' Access files in the current package
#'
#' @param ... character vectors specifying subdirectories and file names within
#'   the package.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "shinydataviewer")
}
