#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Declare your functions
SEXP R_diff_file(SEXP ofile, SEXP nfile, SEXP difffile, SEXP funccontext, SEXP ctxlen, SEXP ignore_whitespace, SEXP algorithm, SEXP indent, SEXP ignore);
SEXP R_merge_file(SEXP base, SEXP merge1, SEXP merge2, SEXP difffile, SEXP level, SEXP flavor, SEXP style);

// Define the call methods table
static const R_CallMethodDef CallEntries[] = {
  {"R_diff_file", (DL_FUNC) &R_diff_file, 9},
  {"R_merge_file", (DL_FUNC) &R_merge_file, 7},
  {NULL, NULL, 0}
};

// Define the registration function
void R_init_yourpackage(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
