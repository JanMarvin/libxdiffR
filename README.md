libxdiffR
================

[![R-CMD-check](https://github.com/JanMarvin/libxdiffR/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/JanMarvin/libxdiffR/actions/workflows/check-standard.yaml)
[![r-universe](https://janmarvin.r-universe.dev/badges/libxdiffR)](https://janmarvin.r-universe.dev/libxdiffR)
[![codecov](https://codecov.io/gh/JanMarvin/libxdiffR/graph/badge.svg?token=9PERJAFW24)](https://codecov.io/gh/JanMarvin/libxdiffR)

`libxdiffR` is an R package that provides a seamless interface to the
powerful `xdiff` library, enabling efficient file and directory
comparison and three-way merging operations within the R environment.
The package includes three primary functions that cater to various diff
and merge scenarios:

- `unidiff()`: A wrapper around the `xdl_diff()` function from the
  `xdiff` library, `unidiff()` allows users to perform a unified diff
  between two files. This function is ideal for identifying differences
  in text files, such as source code or configuration files, and outputs
  the differences in the widely recognized unified diff format.

- `unidiff_dir()`: This function extends the capabilities of `unidiff()`
  by enabling the comparison of entire directories. It recursively
  compares files within two directories, providing a detailed report of
  additions, deletions, and modifications. This is particularly useful
  for version control, backup verification, or synchronization tasks.

- `merge3()`: A wrapper around the `xdl_merge()` function, `merge3()`
  facilitates three-way merging of files. This function is designed to
  handle complex merge scenarios, such as those encountered in
  collaborative development environments where multiple contributors may
  modify the same file. `merge3()` efficiently merges changes from a
  common ancestor file with two modified versions, helping to resolve
  conflicts and create a consolidated output.

This package bundles the `xdiff` library from git. Originally, `xdiff`
was an independent project (<http://www.xmailserver.org/xdiff-lib.html>)
that was later bundled by the git project. The library has since been
unbundled by the libgit2 project, and our sources are derived from the
current version available at <https://github.com/libgit2/xdiff>.

With `libxdiffR`, R users can easily incorporate robust diff and merge
functionalities into their workflows, making it an essential tool for
developers, data scientists, and anyone who needs to manage and compare
file versions or directories in R.

## Installation

You can install the development version of `libxdiffR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("JanMarvin/libxdiffR")
```

Or from [r-universe](https://r-universe.dev/) with:

``` r
# Enable repository from janmarvin
options(repos = c(
  janmarvin = 'https://janmarvin.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install libxdiffR in R
install.packages('libxdiffR')
```

## Example

This package can be used with
[`diffr2`](https://github.com/JanMarvin/diffr2) as a replacement for the
jsdiff tool.

``` r
old <- "This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.
This paragraph contains text that is outdated. It will be deleted in the near future.
It is important to spell check this dokument. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it."

new <- "This is an important notice! It should therefore be located at the beginning of this document!
This part of the document has stayed the same from version to version.  It shouldn't be shown if it doesn't change.  Otherwise, that would not be helping to compress the size of the changes.
It is important to spell check this document. On the other hand, a misspelled word isn't the end of the world. Nothing in the rest of this paragraph needs to be changed. Things can be added after it.
This paragraph contains important new additions to this document."

got <- libxdiffR::unidiff(old, new, create_head = TRUE)

diffr2::diffr2(got)
```
