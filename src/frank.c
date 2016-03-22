#include "data.table.h"
#include <Rdefines.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

SEXP frank(SEXP xorderArg, SEXP xstartArg, SEXP xlenArg, SEXP ties_method) {
    int i=0, j=0, k=0, n;
    int *xstart = INTEGER(xstartArg), *xlen = INTEGER(xlenArg), *xorder = INTEGER(xorderArg);
    enum {MEAN, MAX, MIN, DENSE, SEQUENCE} ties = MEAN; // RUNLENGTH
    SEXP ans;

    if (!strcmp(CHAR(STRING_ELT(ties_method, 0)), "average"))  ties = MEAN;
    else if (!strcmp(CHAR(STRING_ELT(ties_method, 0)), "max")) ties = MAX;
    else if (!strcmp(CHAR(STRING_ELT(ties_method, 0)), "min")) ties = MIN;
    else if (!strcmp(CHAR(STRING_ELT(ties_method, 0)), "dense")) ties = DENSE;
    else if (!strcmp(CHAR(STRING_ELT(ties_method, 0)), "sequence")) ties = SEQUENCE;
    // else if (!strcmp(CHAR(STRING_ELT(ties_method, 0)), "runlength")) ties = RUNLENGTH;
    else error("Internal error: invalid ties.method for frankv(), should have been caught before. Please report to datatable-help");
    n = length(xorderArg);
    ans = (ties == MEAN) ? PROTECT(allocVector(REALSXP, n)) : PROTECT(allocVector(INTSXP, n));
    if (n > 0) {
        switch (ties) {
            case MEAN : 
            for (i = 0; i < length(xstartArg); i++) {
                for (j = xstart[i]-1; j < xstart[i]+xlen[i]-1; j++)
                    REAL(ans)[xorder[j]-1] = (2*xstart[i]+xlen[i]-1)/2.0;
            }
            break;
            case MAX :
            for (i = 0; i < length(xstartArg); i++) {
                for (j = xstart[i]-1; j < xstart[i]+xlen[i]-1; j++)
                    INTEGER(ans)[xorder[j]-1] = xstart[i]+xlen[i]-1;
            }
            break;
            case MIN :
            for (i = 0; i < length(xstartArg); i++) {
                for (j = xstart[i]-1; j < xstart[i]+xlen[i]-1; j++)
                    INTEGER(ans)[xorder[j]-1] = xstart[i];
            }
            break;
            case DENSE :
            k=1;
            for (i = 0; i < length(xstartArg); i++) {
                for (j = xstart[i]-1; j < xstart[i]+xlen[i]-1; j++)
                    INTEGER(ans)[xorder[j]-1] = k;
                k++;
            }
            break;
            case SEQUENCE :
            for (i = 0; i < length(xstartArg); i++) {
                k=1;
                for (j = xstart[i]-1; j < xstart[i]+xlen[i]-1; j++)
                    INTEGER(ans)[xorder[j]-1] = k++;
            }
            break;
            // case RUNLENGTH :
            // for (i = 0; i < length(xstartArg); i++) {
            //     k=1;
            //     for (j = xstart[i]-1; j < xstart[i]+xlen[i]-1; j++)
            //         INTEGER(ans)[xorder[j]-1] = k++;
            // }
            break;
        }
    }
    UNPROTECT(1);
    return(ans);
}
