#' create unified diffs
#' @param old oldfile
#' @param new oldfile
#' @param create_head add a header to the file
#' @useDynLib libxdiffR, .registration=TRUE
#'
#' @import Rcpp
#' @export
unidiff <- function(old, new, create_head = TRUE) {

  old_name <- "old"
  new_name <- "new"

  if (file.exists(old) || file.exists(new)) {
    old_name <- old
    old <- paste0(readLines(old, warn = FALSE), collapse = "\n")
    new_name <- new
    new <- paste0(readLines(new, warn = FALSE), collapse = "\n")
  }

  head <- sprintf(
    "===================================================================\n--- %s\n+++ %s\n",
    old_name,
    new_name
  )

  tmp <- tempfile()
  diff_file(old, new, tmp)

  body <- paste0(readLines(tmp), collapse = "\n")

  if (create_head) {
    body <- paste0(head, body)
  }

  body
}
