#' @useDynLib libxdiffR R_diff_file
diff_file <- function(ofile, nfile, difffile){
  stopifnot(is.character(ofile))
  stopifnot(is.character(nfile))
  stopifnot(is.character(difffile))
  .Call(R_diff_file, ofile, nfile, difffile)
}

#' create unified diffs
#' @param old oldfile
#' @param new oldfile
#' @param create_head add a header to the file
#' @export
unidiff <- function(old, new, create_head = TRUE) {

  old_name <- "old"
  new_name <- "new"

  if (file.exists(old)) {
    old_name <- old
    old <- paste0(readLines(old, warn = FALSE), collapse = "\n")
  }

  if (file.exists(new)) {
    new_name <- new
    new <- paste0(readLines(new, warn = FALSE), collapse = "\n")
  }

  if (is.na(old)) {
    old_name <- "/dev/null"
    old <- ""
  }

  if (is.na(new)) {
    new_name <- "/dev/null"
    new <- ""
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
