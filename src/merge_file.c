#define R_NO_REMAP
#define STRICT_R_HEADERS
#include <Rinternals.h>

#include "xdiff/xdiff.h"
#include <string.h>
#include <stdio.h>

SEXP R_merge_file(SEXP base, SEXP merge1, SEXP merge2, SEXP difffile, SEXP level, SEXP favor, SEXP style) {

  // Extract strings from SEXP objects
  const char *ofile_str    = CHAR(STRING_ELT(base, 0));
  const char *m1file_str   = CHAR(STRING_ELT(merge1, 0));
  const char *m2file_str   = CHAR(STRING_ELT(merge2, 0));
  const char *difffile_str = CHAR(STRING_ELT(difffile, 0));

  xmparam_t xmp;
  FILE* outfile = fopen(difffile_str, "w");

  if(outfile == NULL) {
    Rf_error("Could not open file %s for writing", difffile_str);
  }

  mmfile_t f0, f1, f2;
  mmbuffer_t mb;

  f0.size = strlen(ofile_str);
  f0.ptr  = (char*)ofile_str;
  f1.size = strlen(m1file_str);
  f1.ptr  = (char*)m1file_str;
  f2.size = strlen(m2file_str);
  f2.ptr  = (char*)m2file_str;

  memset(&xmp, 0, sizeof(xmp));

  int level_value = INTEGER(level)[0];
  int favor_value = INTEGER(favor)[0];
  int style_value = INTEGER(style)[0];

  xmp.level = level_value;
  xmp.style = style_value;
  xmp.favor = favor_value;
  xmp.ancestor = "Base";
  xmp.file1 = "Head";
  xmp.file2 = "Incoming";

  int result = xdl_merge(&f0, &f1, &f2, &xmp, &mb);

  // Rprintf("result: %d\n", result);
  fprintf(outfile, "%.*s", (int) mb.size, mb.ptr);

  fclose(outfile);

  return Rf_ScalarInteger(result);
}
