if (!exists("summarize_columns", mode = "function")) {
  package_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  r_dir <- file.path(package_root, "R")

  for (file in list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)) {
    source(file)
  }
}

normalize_snapshot_html <- function(x) {
  x <- gsub("bslib-card-[0-9]+", "bslib-card-<id>", x, perl = TRUE)
  x <- gsub("bslib-accordion-panel-[0-9]+", "bslib-accordion-panel-<id>", x, perl = TRUE)
  x <- gsub(
    "class=\"bi bi-chevron-left collapse-icon\" style=\"[^\"]*\"",
    "class=\"bi bi-chevron-left collapse-icon\" style=\"<normalized>\"",
    x,
    perl = TRUE
  )
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}
