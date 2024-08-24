#' @useDynLib libxdiffR R_merge_file
merge_file <- function(base, merge1, merge2, difffile, level, favor, style) {
  stopifnot(is.character(base))
  stopifnot(is.character(merge1))
  stopifnot(is.character(merge2))
  stopifnot(is.character(difffile))
  stopifnot(is.integer(level))
  stopifnot(is.integer(favor))
  stopifnot(is.integer(style))
  .Call(R_merge_file, base, merge1, merge2, difffile, level, favor, style)
}

#' create three way merges
#' @param base base file
#' @param merge1,merge2 file with changes
#' @param level "minimal", "eager", "zealous", "zealous_alnum"
#' @param favor merge favor, "ours", "theirs", or "union"
#' @param style merge style, "diff3", or "zdiff3"
#' @importFrom utils download.file
#' @export
merge3 <- function(base, merge1, merge2, level = "minimal", favor = NULL, style = NULL) {

  if (!is.character(base) || !is.character(merge1) || !is.character(merge2)) {
    stop("base, merge1, and merge2 must be characters")
  }

  base   <- force(base)
  merge1 <- force(merge1)
  merge2 <- force(merge2)

  if (grepl("^http|^https", base)) {
    tmp_base <- tempfile()
    download.file(url = base, destfile = tmp_base, cacheOK = FALSE, mode = "w", quiet = TRUE)
    nams_base <- names(base)
    base <- paste0(readLines(tmp_base, warn = FALSE), collapse = "\n")
    names(base) <- nams_base
  }

  if (grepl("^http|^https", merge1)) {
    tmp_merge1 <- tempfile()
    download.file(url = merge1, destfile = tmp_merge1, cacheOK = FALSE, mode = "w", quiet = TRUE)
    nams_merge1 <- names(merge1)
    merge1 <- paste0(readLines(tmp_merge1, warn = FALSE), collapse = "\n")
    names(merge1) <- nams_merge1
  }

  if (grepl("^http|^https", merge2)) {
    tmp_merge2 <- tempfile()
    download.file(url = merge2, destfile = tmp_merge2, cacheOK = FALSE, mode = "w", quiet = TRUE)
    nams_merge2 <- names(merge2)
    merge2 <- paste0(readLines(tmp_merge2, warn = FALSE), collapse = "\n")
    names(merge2) <- nams_merge2
  }

  base_name   <- ifelse(is.null(names(base)),   "base",   names(base))
  merge1_name <- ifelse(is.null(names(merge1)), "merge1", names(merge1))
  merge2_name <- ifelse(is.null(names(merge2)), "merge2", names(merge2))

  if (file.exists(base)) {
    if (is.null(names(base)) && base_name == "base") base_name <- base
    base <- paste0(readLines(base, warn = FALSE), collapse = "\n")
  }

  if (file.exists(merge1)) {
    if (is.null(names(merge1)) && merge1_name == "merge1") merge1_name <- merge1
    merge1 <- paste0(readLines(merge1, warn = FALSE), collapse = "\n")
  }

  if (file.exists(merge2)) {
    if (is.null(names(merge2)) && merge2_name == "merge2") merge2_name <- merge2
    merge2 <- paste0(readLines(merge2, warn = FALSE), collapse = "\n")
  }

  level <- switch (
    level,
    "minimal"        = 0L, # XDL_MERGE_MINIMAL
    "eager"          = 1L, # XDL_MERGE_EAGER
    "zealous"        = 2L, # XDL_MERGE_ZEALOUS
    "zealous_alnum"  = 3L  # XDL_MERGE_ZEALOUS_ALNUM
  )

  if (is.null(favor)) favor <- "null"

  favor <- switch (
    favor,
    "null"   = 0L,
    "ours"   = 1L, # XDL_MERGE_FAVOR_OURS
    "theirs" = 2L, # XDL_MERGE_FAVOR_THEIRS
    "union"  = 3L  # XDL_MERGE_FAVOR_UNION
  )

  if (is.null(style)) style <- "null"

  style <- switch (
    style,
    "null"   = 0L,
    "diff3"  = 1L, # XDL_MERGE_DIFF3
    "zdiff3" = 2L  # XDL_MERGE_ZEALOUS_DIFF3
  )

  tmp <- tempfile()
  merge_file(base, merge1, merge2, tmp, level, favor, style)

  body <- paste0(readLines(tmp, warn = FALSE), collapse = "\n")

  body
}
