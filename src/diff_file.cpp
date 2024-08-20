#include <Rcpp/Lightest>
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

  mmfile_t f1, f2;

  f1.size = ofile.size();
  f1.ptr  = ofile.data();
  f2.size = nfile.size();
  f2.ptr  = nfile.data();

  memset(&ecb, 0, sizeof(ecb));
  memset(&xpp, 0, sizeof(xpp));
  memset(&xecfg, 0, sizeof(xecfg));

  xpp.flags = 0; // allow various flags:
  xecfg.ctxlen = 3;
  ecb.out_line = write_diff;
  ecb.priv = (void *) outfile;

  int out = xdl_diff(&f1, &f2, &xpp, &xecfg, &ecb);
  fclose(outfile);

  return out;
}
