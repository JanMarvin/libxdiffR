#include <Rcpp.h>
#include <xdiff.h>

int seen_hunk;


void read_mmfile(mmfile_t *mf, std::string path) {
  mf->ptr = (char*)path.c_str();
  mf->size = path.size();
}

static int write_diff(void *file, mmbuffer_t *mb, int nbuf)
{
  int i;
  for (i = 0; i < nbuf; i++)
    fprintf((FILE *) file, "%.*s", (int) mb[i].size, mb[i].ptr);
  return 0;
}

// [[Rcpp::export]]
int test(std::string ofile, std::string nfile, std::string argv) {

  xdemitcb_t ecb;
  xpparam_t xpp;
  xdemitconf_t xecfg;
  FILE* outfile = fopen("/tmp/test.diff", "w");

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
  read_mmfile(&f1, ofile);
  read_mmfile(&f2, nfile);

  seen_hunk = 0;

  memset(&xpp, 0, sizeof(xpp));
  xpp.flags = XDF_PATIENCE_DIFF;
  memset(&xecfg, 0, sizeof(xecfg));
  xecfg.ctxlen = 3;
  ecb.outf = write_diff;
  ecb.priv = (void *) outfile;

  int out = xdl_diff(&f1, &f2, &xpp, &xecfg, &ecb);
  fclose(outfile);

//   free(f1.ptr);
//   free(f2.ptr);

  return out;
}
