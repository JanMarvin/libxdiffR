#' @useDynLib libxdiffR R_diff_file
diff_file <- function(ofile, nfile, difffile, funccontext, cntxtlen, ignore_whitespace, algorithm, indent) {
  stopifnot(is.character(ofile))
  stopifnot(is.character(nfile))
  stopifnot(is.character(difffile))
  stopifnot(is.logical(funccontext))
  stopifnot(is.integer(cntxtlen))
  stopifnot(is.integer(ignore_whitespace))
  stopifnot(is.integer(algorithm))
  stopifnot(is.logical(indent))
  .Call(R_diff_file, ofile, nfile, difffile, funccontext, cntxtlen, ignore_whitespace, algorithm, indent)
}

#' create unified diffs
#' @param old oldfile
#' @param new oldfile
#' @param create_head add a header to the file
#' @param with_context add full context
#' @param context_length length of context
#' @param ignore_whitespace used a selection of c("all", "change", "at_eol", "cr_at_eol", "blank_lines")
#' @param algorithm "minimal", "patience", or "histogram"
#' @param indent_heuristic something ...
#' @importFrom utils download.file
#' @export
unidiff <- function(old, new, create_head = TRUE, with_context = FALSE, context_length = 3, ignore_whitespace = NULL, algorithm = "minimal", indent_heuristic = FALSE) {

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

  iws <- vector("integer", length = 5)

  ignore_whitespace <- tolower(ignore_whitespace)
  if ("all" %in% ignore_whitespace)         iws[1] <- 1L
  if ("change" %in% ignore_whitespace)      iws[2] <- 1L
  if ("at_eol" %in% ignore_whitespace)      iws[3] <- 1L
  if ("cr_at_eol" %in% ignore_whitespace)   iws[4] <- 1L
  if ("blank_lines" %in% ignore_whitespace) iws[5] <- 1L

  algorithm <- switch(
    tolower(algorithm),
    "minimal"   = 0L, # the default
    "patience"  = 1L,
    "histogram" = 2L
  )

  tmp <- tempfile()
  if (diff_file(old, new, tmp, with_context, as.integer(context_length), iws, algorithm, indent_heuristic))
    stop("unable to create diff")

  body <- paste0(readLines(tmp), collapse = "\n")

  if (create_head) {
    body <- paste0(head, body)
  }

  body
}
