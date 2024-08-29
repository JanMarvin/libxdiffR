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

SEXP R_diff_file(SEXP ofile, SEXP nfile, SEXP difffile, SEXP funccontext, SEXP ctxlen, SEXP ignore_whitespace, SEXP algorithm, SEXP indent, SEXP ignore) {

  int with_context   = LOGICAL(funccontext)[0];
  int contextlen     = INTEGER(ctxlen)[0];
  int ws_all         = INTEGER(ignore_whitespace)[0];
  int ws_change      = INTEGER(ignore_whitespace)[1];
  int ws_at_eol      = INTEGER(ignore_whitespace)[2];
  int ws_cr_at_eol   = INTEGER(ignore_whitespace)[3];
  int ws_blank_lines = INTEGER(ignore_whitespace)[4];
  int algo           = INTEGER(algorithm)[0];

  int idheuristic    = INTEGER(indent)[0];


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

  // algorithm
  if (algo == 1) {
    xpp.flags |= XDF_PATIENCE_DIFF;
  }
  if (algo == 2) {
    xpp.flags |= XDF_HISTOGRAM_DIFF;
  }

  // whitespace
  if (ws_all == 1) {
    xpp.flags |= XDF_IGNORE_WHITESPACE;
  }

  if (ws_change == 1) {
    xpp.flags |= XDF_IGNORE_WHITESPACE_CHANGE;
  }

  if (ws_at_eol == 1) {
    xpp.flags |= XDF_IGNORE_WHITESPACE_AT_EOL;
  }

  if (ws_cr_at_eol == 1) {
    xpp.flags |= XDF_IGNORE_CR_AT_EOL;
  }

  if (ws_blank_lines == 1) {
    xpp.flags |= XDF_IGNORE_BLANK_LINES;
  }

  if (idheuristic) {
    xpp.flags |= XDF_INDENT_HEURISTIC;
  }

  regex_t regex;
  regex_t *regex_ptr __attribute__((unused)) = &regex;

  if (TYPEOF(ignore) != STRSXP || LENGTH(ignore) != 1) {
    Rf_error("Pattern must be a single string.");
  }
  const char *pttrn = CHAR(STRING_ELT(ignore, 0));

  if (strcmp(pttrn, "") != 0) {
#ifdef WITHOUT_REGEX_H
      Rf_error("Regex is not supported on this platform, but a non-empty pattern was provided.");
#else
      int ret = regcomp(&regex, pttrn, REG_EXTENDED);
      if (ret) Rf_error("Failed to compile regex pattern.");
      xpp.ignore_regex = &regex_ptr;
      xpp.ignore_regex_nr = 1;
#endif
  }

  xecfg.ctxlen = contextlen;
  xecfg.flags = XDL_EMIT_FUNCNAMES;
  if (with_context) {
    xecfg.flags |= XDL_EMIT_FUNCCONTEXT;
  }
  ecb.out_line = write_diff;
  ecb.priv = (void *) outfile;

  int out = xdl_diff(&f1, &f2, &xpp, &xecfg, &ecb);
  fclose(outfile);

#ifndef WITHOUT_REGEX_H
  if (strcmp(pttrn, "") != 0)
    regfree(regex_ptr);
#endif

  return Rf_ScalarInteger(out);
}
