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

#' Compare Files Using Unified Diffs
#'
#' `unidiff()` is a function designed to generate unified diffs between two files or text inputs. Unified diffs are widely used in version control systems to highlight differences between two versions of a file in a compact and readable format.
#'
#' The `unidiff()` function compares two inputs—whether they are files, URLs, or raw text—and generates a unified diff that highlights the differences between them. It supports advanced features like customizable context, whitespace handling, and different diff algorithms to suit various needs. The function can also handle inputs from URLs, making it flexible for remote file comparisons.
#' The output is a unified diff string, which can be directly used in version control systems or for manual inspection of changes.
#' For identical files, no diff is returned, and an empty character string is provided.
#' @param old The original file or text input. This can be a file path, a URL, or a string of text.
#' @param new The modified file or text input to compare against the original. This can also be a file path, a URL, or a string of text.
#' @param create_head A logical flag (default `TRUE`) indicating whether to include a header in the diff output. The header contains the filenames or identifiers of the files being compared.
#' @param with_context A logical flag (default `FALSE`) specifying whether to include the full context in the diff output. Including context helps to provide a more complete view of the changes.
#' @param context_length An integer value (default `3`) that determines the number of lines of context to include around each change. This is only relevant if with_context is `TRUE`.
#' @param ignore_whitespace A character vector indicating how whitespace differences should be treated. Options include `"all"`, `"change"`, `"at_eol"`, `"cr_at_eol"`, and `"blank_lines"`, allowing for fine-grained control over which whitespace differences to ignore.
#' @param algorithm  A string specifying the diff algorithm to use. Options are `"minimal"`, `"patience"`, and `"histogram"`, with `"minimal"` being the default. Each algorithm has different characteristics suited to various types of text comparisons.
#' @param indent_heuristic A logical flag (default `FALSE`) that, when enabled, applies an additional heuristic to better handle indented lines, improving the accuracy of the diff in some cases.
#' @importFrom utils download.file
#' @examples
#' cat(unidiff(old = "foo bar", new = "foo baz"))
#' @return A character string containing the unified diff.
#' @export
unidiff <- function(old, new, create_head = TRUE, with_context = FALSE, context_length = 3, ignore_whitespace = NULL, algorithm = "minimal", indent_heuristic = FALSE) {

  if (!is.character(old) || !is.character(new)) {
    stop("old and new must be characters")
  }

  old <- force(old)
  new <- force(new)

  old_is_url <- FALSE
  new_is_url <- FALSE

  if (grepl("^http|^https", old)) {
    tmp_old <- tempfile()
    download.file(url = old, destfile = tmp_old, cacheOK = FALSE, mode = "w", quiet = TRUE)
    nams_old <- names(old)
    old <- paste0(readLines(tmp_old, warn = FALSE), collapse = "\n")
    names(old) <- nams_old
    old_is_url <- TRUE
  }

  if (grepl("^http|^https", new)) {
    tmp_new <- tempfile()
    download.file(url = new, destfile = tmp_new, cacheOK = FALSE, mode = "w", quiet = TRUE)
    nams_new <- names(new)
    new <- paste0(readLines(new, warn = FALSE), collapse = "\n")
    names(new) <- nams_new
    new_is_url <- TRUE
  }

  old_name <- ifelse(is.null(names(old)), "old", names(old))
  new_name <- ifelse(is.null(names(new)), "new", names(new))

  if (!old_is_url && file.exists(path.expand(old))) {
    if (is.null(names(old)) && old_name == "old") old_name <- old
    old <- paste0(readLines(path.expand(old), warn = FALSE), collapse = "\n")
  }

  if (!new_is_url && file.exists(path.expand(new))) {
    if (is.null(names(new)) && new_name == "new") new_name <- new
    new <- paste0(readLines(path.expand(new), warn = FALSE), collapse = "\n")
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

  if (create_head && nchar(body)) {
    body <- paste0(head, body)
  }

  body
}
