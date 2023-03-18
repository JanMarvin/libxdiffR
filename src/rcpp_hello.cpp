#include <Rcpp.h>
#include "xdiff/xdiff.h"

static int write_diff(void *file, mmbuffer_t *mb, int nbuf)
{
  int i;
  for (i = 0; i < nbuf; i++)
    fprintf((FILE *) file, "%.*s", (int) mb[i].size, mb[i].ptr);
  return 0;
}

// [[Rcpp::export]]
int diff_file(std::string ofile, std::string nfile, std::string difffile) {

  xdemitcb_t ecb;
  xpparam_t xpp;
  xdemitconf_t xecfg;
  FILE* outfile = fopen(difffile.c_str(), "w");

  // xpparam_t param = { 1 };
  // xdemitconf_t econf = { 3 };
  // xdemitcb_t ecb = { NULL, NULL, };
  // ecb.priv = stdout;
  // econf.ctxlen = 20;

  // FILE *fd;
  // fd = fopen("/tmp/test.diff", "a");
  // ecb.priv = fd;
  // ecb.outf = fn_out;

  // param.flags = XDF_NEED_MINIMAL;

  mmfile_t f1, f2;

  f1.size = ofile.size();
  f1.ptr =  (char*)ofile.c_str();
  f2.size = nfile.size();
  f2.ptr = (char*)nfile.c_str();

  memset(&xpp, 0, sizeof(xpp));
  xpp.flags = 0;
  memset(&xecfg, 0, sizeof(xecfg));
  xecfg.ctxlen = 3;
  ecb.outf = write_diff;
  ecb.priv = (void *) outfile;

  int out = xdl_diff(&f1, &f2, &xpp, &xecfg, &ecb);
  fclose(outfile);

  return out;
}
