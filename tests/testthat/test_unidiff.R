test_that("unidiff works", {
  old <- "This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.
This paragraph contains text that is outdated. It will be deleted in the near future.
It is important to spell check this dokument. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it."

  new <- "This is an important notice! It should therefore be located at the beginning of this document!
This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.
It is important to spell check this document. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.
This paragraph contains important new additions to this document."

  got <- unidiff(old, new, create_head = FALSE)

  exp <- "@@ -1,3 +1,4 @@\n+This is an important notice! It should therefore be located at the beginning of this document!\n This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.\n-This paragraph contains text that is outdated. It will be deleted in the near future.\n-It is important to spell check this dokument. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.\n\\ No newline at end of file\n+It is important to spell check this document. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.\n+This paragraph contains important new additions to this document.\n\\ No newline at end of file"
  expect_equal(got, exp)

  got <- unidiff(old, new, create_head = TRUE)

  exp <- "===================================================================\n--- old\n+++ new\n@@ -1,3 +1,4 @@\n+This is an important notice! It should therefore be located at the beginning of this document!\n This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.\n-This paragraph contains text that is outdated. It will be deleted in the near future.\n-It is important to spell check this dokument. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.\n\\ No newline at end of file\n+It is important to spell check this document. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.\n+This paragraph contains important new additions to this document.\n\\ No newline at end of file"
  expect_equal(got, exp)
})

test_that("unidiff works", {
  set.seed(123)
  old <- "This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.
This paragraph contains text that is outdated. It will be deleted in the near future.
It is important to spell check this dokument. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it."

  new <- "This is an important notice! It should therefore be located at the beginning of this document!
This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.
It is important to spell check this document. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.
This paragraph contains important new additions to this document."

  temp1 <- "test_file1.txt"
  temp2 <- "test_file2.txt"

  writeLines(old, temp1)
  writeLines(new, temp2)

  got <- unidiff(temp1, temp2, create_head = FALSE)

  exp <- "@@ -1,3 +1,4 @@\n+This is an important notice! It should therefore be located at the beginning of this document!\n This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.\n-This paragraph contains text that is outdated. It will be deleted in the near future.\n-It is important to spell check this dokument. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.\n\\ No newline at end of file\n+It is important to spell check this document. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.\n+This paragraph contains important new additions to this document.\n\\ No newline at end of file"
  expect_equal(got, exp)


  got <- unidiff(temp1, temp2, create_head = TRUE)

  exp <- exp <- "===================================================================\n--- test_file1.txt\n+++ test_file2.txt\n@@ -1,3 +1,4 @@\n+This is an important notice! It should therefore be located at the beginning of this document!\n This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.\n-This paragraph contains text that is outdated. It will be deleted in the near future.\n-It is important to spell check this dokument. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.\n\\ No newline at end of file\n+It is important to spell check this document. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.\n+This paragraph contains important new additions to this document.\n\\ No newline at end of file"
  expect_equal(got, exp)

  unlink(temp1)
  unlink(temp2)
})

test_that("use names", {

  exp <- "===================================================================\n--- a\n+++ a\n@@ -1 +1 @@\n-a b\n\\ No newline at end of file\n+a  b\n\\ No newline at end of file"
  got <- unidiff(old = c(a = "a b"), new = c(a = "a  b"))
  expect_equal(exp, got)

  set.seed(123)
  old <- "This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.
This paragraph contains text that is outdated. It will be deleted in the near future.
It is important to spell check this dokument. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it."

  new <- "This is an important notice! It should therefore be located at the beginning of this document!
This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.
It is important to spell check this document. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.
This paragraph contains important new additions to this document."

  temp1 <- "test_file1.txt"
  temp2 <- "test_file2.txt"

  writeLines(old, temp1)
  writeLines(new, temp2)

  exp <- "===================================================================\n--- old\n+++ new\n@@ -1,3 +1,4 @@\n+This is an important notice! It should therefore be located at the beginning of this document!\n This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.\n-This paragraph contains text that is outdated. It will be deleted in the near future.\n-It is important to spell check this dokument. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.\n\\ No newline at end of file\n+It is important to spell check this document. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.\n+This paragraph contains important new additions to this document.\n\\ No newline at end of file"
  got <- unidiff(c(old = temp1), c(new = temp2), create_head = TRUE)
  expect_equal(exp, got)

  unlink(temp1)
  unlink(temp2)

})

test_that("errors work", {

  expect_error(unidiff(old = 1, new = c(a = "a  b")), "old and new must be characters")
  expect_error(unidiff(new = 1, old = c(a = "a  b")), "old and new must be characters")

})

test_that("downloads work", {

  skip_if_offline()

  exp <- "===================================================================\n--- old\n+++ new\n   for (i in seq_len(nSheets)) {\n     if (sheets$typ[i] == \"worksheet\") {\n       if (length(wb$worksheets[[i]]$headerFooter)) {\n-        wb$worksheets[[i]]$headerFooter <- getHeaderFooterNode(wb$worksheets[[i]]$headerFooter)\n+        # get attributes\n+        hf_attrs <- rbindlist(xml_attr(wb$worksheets[[i]]$headerFooter, \"headerFooter\"))\n+\n+        if (!is.null(hf_attrs$alignWithMargins))\n+          wb$worksheets[[i]]$align_with_margins <- as.logical(as.integer(hf_attrs$alignWithMargins))\n+\n+        if (!is.null(hf_attrs$scaleWithDoc))\n+          wb$worksheets[[i]]$scale_with_doc     <- as.logical(as.integer(hf_attrs$scaleWithDoc))\n+\n+        wb$worksheets[[i]]$headerFooter       <- getHeaderFooterNode(wb$worksheets[[i]]$headerFooter)\n       }\n     }\n   }"
  got <- unidiff(
    old = "https://raw.githubusercontent.com/JanMarvin/openxlsx2/8b99b77ebf00a89da07d4b81542efaba51551cd9/R/wb_load.R",
    new = "https://raw.githubusercontent.com/JanMarvin/openxlsx2/228c51fbe2b0da74b434ca5f2a4aa2c833397bd6/R/wb_load.R"
  )
  expect_equal(exp, got)

})
