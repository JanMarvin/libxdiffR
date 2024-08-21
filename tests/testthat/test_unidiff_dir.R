test_that("unidiff_dir() works", {

  # a bit overkill
  library(fs)       # For working with the file system
  library(stringi)  # For modifying strings to introduce differences

  write_file <- function(x, path, append = FALSE) {
    if (append) {
      x <- c(readLines(path, warn = FALSE), x)
    }
    writeLines(x, path)
  }

  set.seed(123)

  prepare_tmpdir <- function(x, create = FALSE) {
    if (dir.exists(x))
      unlink(x, recursive = TRUE)
    if (create) dir.create(x)
    x
  }

  # Function to create the initial directory with files
  create_initial_dir <- function(directory) {
    # Create nested subdirectories
    subdirs <- c("subdir1", "subdir2", "subdir3/nested_subdir")

    for (subdir in subdirs) {
      dir_create(file.path(directory, subdir))
    }

    # Define file names
    files <- paste0("file", 1:5, ".txt")

    # Function to create unique text content for each file
    generate_unique_content <- function(filename, subdir) {
      paste0("This is a unique content for ", filename, " in ", subdir, ".\n", stri_rand_lipsum(1))
    }

    # Create files in subdirectories
    for (subdir in subdirs) {
      for (file in files) {
        content <- generate_unique_content(file, subdir)
        write_file(content, file.path(directory, subdir, file))
      }
    }
  }

  # Function to modify dir2 after copying from dir1
  modify_dir2 <- function(directory) {
    # Modify some files in dir2
    modified_files <- c("subdir1/file1.txt", "subdir2/file2.txt")
    for (file in modified_files) {
      write_file("This content has been modified.", file.path(directory, file), append = TRUE)
    }

    # Remove some files from dir2
    removed_files <- c("subdir2/file3.txt")
    file_delete(file.path(directory, removed_files))

    # Add new files to dir2
    new_files <- c("subdir1/new_file1.txt", "subdir2/new_file2.txt")
    for (file in new_files) {
      write_file("This is a new file.", file.path(directory, file))
    }

    # Rename a file in dir2
    file_move(file.path(directory, "subdir2/file4.txt"), file.path(directory, "subdir2/renamed_file4.txt"))
  }



  # Example usage

  # Create two temporary directories
  tempdir1 <- paste0(testthat::test_path(), "/dir1")
  tempdir2 <- paste0(testthat::test_path(), "/dir2")
  dir1 <- prepare_tmpdir(tempdir1, create = TRUE)
  dir2 <- prepare_tmpdir(tempdir2, create = FALSE)

  dir_create(dir1)
  create_initial_dir(dir1)
  dir_copy(dir1, dir2)

  modify_dir2(dir2)

  tempdir()

  exp <- list(
    c("===================================================================",
      "--- tests/testthat/dir1/subdir1/file1.txt", "+++ tests/testthat/dir2/subdir1/file1.txt",
      "@@ -1,2 +1,3 @@", " This is a unique content for file1.txt in subdir1.",
      "-Lorem ipsum dolor sit amet, sollicitudin duis maecenas habitasse ultrices aenean tempus. Volutpat id, non natoque ad, commodo suscipit sed risus, facilisis mauris aliquam, a. Non leo leo, sapien non eu a quam. Nunc vivamus in, purus ultricies ac suspendisse at. Eu quisque fames sapien consequat et nisl nunc, viverra est mattis mattis posuere. Purus quisque auctor aenean sed risus mauris ante nisi. Ligula ac vitae lacinia. Magna aliquet et mi cubilia per. Hendrerit amet eu ullamcorper turpis ultrices aliquam.",
      "\\ No newline at end of file", "+Lorem ipsum dolor sit amet, sollicitudin duis maecenas habitasse ultrices aenean tempus. Volutpat id, non natoque ad, commodo suscipit sed risus, facilisis mauris aliquam, a. Non leo leo, sapien non eu a quam. Nunc vivamus in, purus ultricies ac suspendisse at. Eu quisque fames sapien consequat et nisl nunc, viverra est mattis mattis posuere. Purus quisque auctor aenean sed risus mauris ante nisi. Ligula ac vitae lacinia. Magna aliquet et mi cubilia per. Hendrerit amet eu ullamcorper turpis ultrices aliquam.",
      "+This content has been modified.", "\\ No newline at end of file",
      "===================================================================",
      "--- /dev/null", "+++ tests/testthat/dir2/subdir1/new_file1.txt",
      "@@ -0,0 +1 @@", "+This is a new file.", "\\ No newline at end of file",
      "===================================================================",
      "--- tests/testthat/dir1/subdir2/file2.txt", "+++ tests/testthat/dir2/subdir2/file2.txt",
      "@@ -1,2 +1,3 @@", " This is a unique content for file2.txt in subdir2.",
      "-Lorem ipsum dolor sit amet, netus tincidunt ut aliquam porttitor purus ultricies. Nulla odio in sit porta venenatis. Dui odio sed non. Molestie ut, vitae facilisi varius eget nisi. Sed non elit in porttitor. Netus et ridiculus sed fermentum lacus felis. Fringilla lectus sodales porttitor. Tincidunt velit vel morbi lobortis quisque felis nulla sodales faucibus turpis, arcu viverra. Tortor vel cras sapien pellentesque rhoncus scelerisque vehicula. Odio sed sem, tempus vehicula porta lectus ut eu eleifend et. Vel nibh cras vitae. Imperdiet ac mollis eget faucibus, sed gravida? Donec inceptos, egestas sociis nisl sem.",
      "\\ No newline at end of file", "+Lorem ipsum dolor sit amet, netus tincidunt ut aliquam porttitor purus ultricies. Nulla odio in sit porta venenatis. Dui odio sed non. Molestie ut, vitae facilisi varius eget nisi. Sed non elit in porttitor. Netus et ridiculus sed fermentum lacus felis. Fringilla lectus sodales porttitor. Tincidunt velit vel morbi lobortis quisque felis nulla sodales faucibus turpis, arcu viverra. Tortor vel cras sapien pellentesque rhoncus scelerisque vehicula. Odio sed sem, tempus vehicula porta lectus ut eu eleifend et. Vel nibh cras vitae. Imperdiet ac mollis eget faucibus, sed gravida? Donec inceptos, egestas sociis nisl sem.",
      "+This content has been modified.", "\\ No newline at end of file",
      "===================================================================",
      "--- tests/testthat/dir1/subdir2/file3.txt", "+++ /dev/null",
      "@@ -1,2 +0,0 @@", "-This is a unique content for file3.txt in subdir2.",
      "-Lorem ipsum dolor sit amet, ac orci suscipit, a, fermentum vehicula. Ac mus vulputate mattis sit sed bibendum, litora eu risus quam. Elit in scelerisque erat lorem fermentum diam. Platea cras venenatis finibus ac vehicula viverra dictumst in vulputate magna. Massa convallis sed sed hac ornare tincidunt mattis. Dictumst vel malesuada mi netus curae lacus a duis. Lectus massa mi justo blandit lacus montes sagittis. Mauris magna magna sed ante justo sit, imperdiet, sed scelerisque. Enim scelerisque sed in magna malesuada mauris proin. Duis potenti leo leo eget sapien. A torquent risus in rhoncus ipsum lectus accumsan nulla in.",
      "\\ No newline at end of file", "===================================================================",
      "--- tests/testthat/dir1/subdir2/file4.txt", "+++ /dev/null",
      "@@ -1,2 +0,0 @@", "-This is a unique content for file4.txt in subdir2.",
      "-Lorem ipsum dolor sit amet, at ultrices. Tortor elementum luctus id. Mauris pharetra, hac ipsum. Aliquam sed, tortor blandit laoreet magnis sem, ut quis fringilla integer. Sagittis cum, ullamcorper sem sit iaculis. Ut lorem habitant sagittis nam ac blandit dictumst nisi nunc. In ut fringilla nibh cum suspendisse arcu, cum ac cursus ante lectus tincidunt. Ut mollis vel ultrices mi, vel hac. Est sed netus semper interdum vestibulum mattis, magnis integer? Eu penatibus suspendisse.",
      "\\ No newline at end of file", "===================================================================",
      "--- /dev/null", "+++ tests/testthat/dir2/subdir2/new_file2.txt",
      "@@ -0,0 +1 @@", "+This is a new file.", "\\ No newline at end of file",
      "===================================================================",
      "--- tests/testthat/dir1/subdir2/file4.txt", "+++ tests/testthat/dir2/subdir2/renamed_file4.txt"
    )
  )
  exp[[1]] <- vapply(exp[[1]], gsub, pattern = "tests/testthat", replacement = testthat::test_path(), FUN.VALUE = NA_character_, USE.NAMES = FALSE)

  got <- unidiff_dir(dir1, dir2)
  got <- strsplit(got, "\n")

  expect_equal(exp, got)

  unlink(dir1, recursive = TRUE)
  unlink(dir2, recursive = TRUE)

})
