libxdiffR
================

[![R-CMD-check](https://github.com/JanMarvin/libxdiffR/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/JanMarvin/libxdiffR/actions/workflows/check-standard.yaml)
[![r-universe](https://janmarvin.r-universe.dev/badges/libxdiffR)](https://janmarvin.r-universe.dev/libxdiffR)
[![codecov](https://codecov.io/gh/JanMarvin/libxdiffR/graph/badge.svg?token=9PERJAFW24)](https://codecov.io/gh/JanMarvin/libxdiffR)

This package bundles the `xdiff` library from `git`. Originally this was
an independent project that was bundled by the git project and has now
been unbundled by the libgit2 project.

Our sources are from here: <https://github.com/libgit2/xdiff>

Currently this package provides now three functions:

``` r
unidiff(old, new)
unidiff_dir(old, new)
merge3(base, ours, theirs)
```

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
