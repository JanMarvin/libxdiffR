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
