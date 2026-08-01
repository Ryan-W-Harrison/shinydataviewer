if (!exists("summarize_columns", mode = "function")) {
  package_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = TRUE
  )
  r_dir <- file.path(package_root, "R")

  for (file in list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)) {
    source(file)
  }
}
