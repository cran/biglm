#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP singcheckQR(void *);
extern SEXP updateQR(void *, void *, void *, void *, void *);

/* .Fortran calls */
extern void F77_NAME(regcf)(void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CallMethodDef CallEntries[] = {
    {"singcheckQR", (DL_FUNC) &singcheckQR, 1},
    {"updateQR",    (DL_FUNC) &updateQR,    5},
    {NULL, NULL, 0}
};

static const R_FortranMethodDef FortranEntries[] = {
    {"regcf", (DL_FUNC) &F77_NAME(regcf), 9},
    {NULL, NULL, 0}
};

void R_init_biglm(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
