#define R_NO_REMAP
#define STRICT_R_HEADERS
#include <Rinternals.h>

#include "xdiff/xdiff.h"
#include <string.h>
#include <stdio.h>

static int write_diff(void *file, mmbuffer_t *mb, int nbuf)
{
  int i;
  for (i = 0; i < nbuf; i++)
    fprintf((FILE *) file, "%.*s", (int) mb[i].size, mb[i].ptr);
  return 0;
}

SEXP R_diff_file(SEXP ofile, SEXP nfile, SEXP difffile) {

  // Extract strings from SEXP objects
  const char *ofile_str = CHAR(STRING_ELT(ofile, 0));
  const char *nfile_str = CHAR(STRING_ELT(nfile, 0));
  const char *difffile_str = CHAR(STRING_ELT(difffile, 0));

  xdemitcb_t ecb;
  xpparam_t xpp;
  xdemitconf_t xecfg;
  FILE* outfile = fopen(difffile_str, "w");

  if(outfile == NULL) {
    Rf_error("Could not open file %s for writing", difffile_str);
  }

  mmfile_t f1, f2;

  f1.size = strlen(ofile_str);
  f1.ptr  = (char*)ofile_str;
  f2.size = strlen(nfile_str);
  f2.ptr  = (char*)nfile_str;

  memset(&ecb, 0, sizeof(ecb));
  memset(&xpp, 0, sizeof(xpp));
  memset(&xecfg, 0, sizeof(xecfg));

  xpp.flags = 0; // allow various flags
  xecfg.ctxlen = 3;
  ecb.out_line = write_diff;
  ecb.priv = (void *) outfile;

  int out = xdl_diff(&f1, &f2, &xpp, &xecfg, &ecb);
  fclose(outfile);

  return Rf_ScalarInteger(out);
}
