#' Function to generate MD5 hashes for all files in a directory
#' @param directory the file directory to create md5 hashes for
#' @importFrom digest digest
#' @inheritParams base::list.files
#' @noRd
generate_md5_df <- function(directory, pattern = NULL) {
  # Get a list of all files in the directory and its subdirectories
  files <- dir(directory, recursive = TRUE, full.names = TRUE)

  # Create a data frame with file names and their MD5 hashes
  md5_df <- data.frame(
    file_path = files,
    file_name = basename(files),
    md5_hash = sapply(files, digest::digest, algo = "md5", file = TRUE),
    stringsAsFactors = FALSE
  )

  md5_df$origin <- gsub(directory, ".", md5_df$file_path)

  md5_df
}


#' Function to compare files between two directories
#' @param old,new directories to check
#' @inheritParams base::list.files
#' @noRd
compare_directories <- function(old, new, pattern = NULL) {
  # Generate MD5 dataframes for both directories
  df1 <- generate_md5_df(old, pattern = pattern)
  df2 <- generate_md5_df(new, pattern = pattern)

  # Full join to compare files based on file name
  all_files <- merge(df1, df2, by = "origin", all = TRUE, suffixes = c("_dir1", "_dir2"))

  # Identify status of each file
  all_files$status <- ifelse(
    is.na(all_files$file_path_dir2),
    "Deleted",
    ifelse(
      is.na(all_files$file_path_dir1),
      "Newly Created",
      ifelse(
        all_files$md5_hash_dir1 != all_files$md5_hash_dir2,
        "Modified",
        "Unchanged"
      )
    )
  )

  # Detect renamed files
  # Select rows where file paths are missing in one of the directories
  possible_renames <- all_files[c(is.na(all_files$file_name_dir1) | is.na(all_files$file_name_dir2)), ]

  # Merge to detect possible renames based on MD5 hash
  possible_renames1 <- possible_renames[!is.na(possible_renames$md5_hash_dir1), c("md5_hash_dir1", "file_name_dir1", "file_path_dir1")]
  possible_renames2 <- possible_renames[!is.na(possible_renames$md5_hash_dir2), c("md5_hash_dir2", "file_name_dir2", "file_path_dir2")]

  rename_matches <- merge(possible_renames1, possible_renames2, by.x = "md5_hash_dir1", by.y = "md5_hash_dir2", all.x = FALSE, all.y = FALSE)
  if (NROW(rename_matches)) rename_matches$status <- "Renamed"

  # Update the main comparison dataframe with rename information
  sel <- match(rename_matches$md5_hash_dir1, all_files$md5_hash_dir2)
  all_files[sel, names(rename_matches)] <- rename_matches

  # Filter out unchanged files and select relevant columns
  final_comparison <- all_files[all_files$status != "Unchanged", c("file_path_dir1", "file_path_dir2", "status")]

  final_comparison
}


#' Compare Files Between Two Directories Using Unified Diffs
#'
#' The `unidiff_dir()` function compares files between two directories and generates unified diffs for each differing file. This function is useful for identifying and displaying differences between two directory structures, such as when comparing different versions of a codebase.
#'
#' @param old The directory path of the first directory to compare.
#' @param new The directory path of the second directory to compare.
#' @param pattern An optional regular expression. Only file names matching the pattern will be included in the comparison. Default is `NULL`, meaning all files are compared.
#' @inheritParams base::list.files
#' @inheritParams unidiff
#'
#' @return A character string containing the unified diffs for all differing files between the two directories.
#' @export
unidiff_dir <- function(old, new, pattern = NULL, create_head = TRUE, with_context = FALSE, context_length = 3, ignore_whitespace = NULL, algorithm = "minimal", indent_heuristic = FALSE, ignore = NULL) {

  comparison_result <- compare_directories(old, new, pattern = pattern)

  diffs <- vapply(seq_len(nrow(comparison_result)), function(x) {
    unidiff(
      old = comparison_result[x, "file_path_dir1"],
      new = comparison_result[x, "file_path_dir2"],
      create_head = create_head,
      with_context = with_context,
      context_length = context_length,
      ignore_whitespace = ignore_whitespace,
      algorithm = algorithm,
      indent_heuristic = indent_heuristic,
      ignore = ignore
    )
  }, NA_character_)

  diff_dir <- paste0(diffs, collapse = "\n")

  diff_dir
}
