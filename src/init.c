#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Declare your functions
SEXP R_diff_file(SEXP ofile, SEXP nfile, SEXP difffile);

// Define the call methods table
static const R_CallMethodDef CallEntries[] = {
  {"R_diff_file", (DL_FUNC) &R_diff_file, 3},
  {NULL, NULL, 0}
};

// Define the registration function
void R_init_yourpackage(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
