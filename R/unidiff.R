#' @useDynLib libxdiffR R_diff_file
diff_file <- function(ofile, nfile, difffile) {
  stopifnot(is.character(ofile))
  stopifnot(is.character(nfile))
  stopifnot(is.character(difffile))
  .Call(R_diff_file, ofile, nfile, difffile)
}

#' create unified diffs
#' @param old oldfile
#' @param new oldfile
#' @param create_head add a header to the file
#' @importFrom utils download.file
#' @export
unidiff <- function(old, new, create_head = TRUE) {

  if (!is.character(old) || !is.character(new)) {
    stop("old and new must be characters")
  }

  old <- force(old)
  new <- force(new)

  if (grepl("^http|^https", old)) {
    tmp_old <- tempfile()
    download.file(url = old, destfile = tmp_old, cacheOK = FALSE, mode = "w", quiet = TRUE)
    nams_old <- names(old)
    old <- paste0(readLines(tmp_old, warn = FALSE), collapse = "\n")
    names(old) <- nams_old
  }

  if (grepl("^http|^https", new)) {
    tmp_new <- tempfile()
    download.file(url = new, destfile = tmp_new, cacheOK = FALSE, mode = "w", quiet = TRUE)
    nams_new <- names(new)
    new <- paste0(readLines(new, warn = FALSE), collapse = "\n")
    names(new) <- nams_new
  }

  old_name <- ifelse(is.null(names(old)), "old", names(old))
  new_name <- ifelse(is.null(names(new)), "new", names(new))

  if (file.exists(old)) {
    if (is.null(names(old)) && old_name == "old") old_name <- old
    old <- paste0(readLines(old, warn = FALSE), collapse = "\n")
  }

  if (file.exists(new)) {
    if (is.null(names(new)) && new_name == "new") new_name <- new
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
