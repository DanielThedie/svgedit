#' Check if file paths exist
#'
#' @param paths A character vector of file paths to check.
#' @returns No return value. Throws an error if any file does not exist.
check_paths <- function(paths) {
  for (path in paths) {
    if (!file.exists(path)) {
      cli::cli_abort(c("x" = "File not found: {path}"))
    }
  }
}
