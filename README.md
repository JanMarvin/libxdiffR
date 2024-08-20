libxdiffR
================

This package bundles the `xdiff` library from `git`. Originally this was
an independent project that was bundled by the git project and has now
been unbundled by the libgit2 project.

Our sources are from here: <https://github.com/libgit2/xdiff>

Currently this package provides just one function:

``` r
unidiff(old, new, create_head = FALSE)
```

Since this package requires `<regex.h>` and the Windows rtools ship this
starting R 4.2, the package cannot be built with R \< 4.2 on Windows. It
might be possible to use the `tre` library to circumvent this issue, but
somebody couldn’t make it work.

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
