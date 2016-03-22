#include "data.table.h"
#include <Rdefines.h>

// plucked and modified from base (coerce.c and summary.c). 
// for melt's `na.rm=TRUE` option
SEXP which_notNA(SEXP x) {
    SEXP v, ans;
    int i, j=0, n = length(x), *buf;
    
    PROTECT(v = allocVector(LGLSXP, n));
    switch (TYPEOF(x)) {
    case LGLSXP:
        for (i = 0; i < n; i++) LOGICAL(v)[i] = (LOGICAL(x)[i] != NA_LOGICAL);
        break;
    case INTSXP:
        for (i = 0; i < n; i++) LOGICAL(v)[i] = (INTEGER(x)[i] != NA_INTEGER);
        break;
    case REALSXP:
        for (i = 0; i < n; i++) LOGICAL(v)[i] = !ISNAN(REAL(x)[i]);
        break;
    case STRSXP:
        for (i = 0; i < n; i++) LOGICAL(v)[i] = (STRING_ELT(x, i) != NA_STRING);
        break;
    default:
        error("%s() applied to non-(list or vector) of type '%s'",
              "which_notNA", type2char(TYPEOF(x)));
    }
    
    buf = (int *) R_alloc(n, sizeof(int));
    for (i = 0; i < n; i++) {
        if (LOGICAL(v)[i] == TRUE) {
            buf[j] = i + 1;
            j++;
        }
    }
    n = j;
    PROTECT(ans = allocVector(INTSXP, n));
    if (n) memcpy(INTEGER(ans), buf, sizeof(int) * n);
    
    UNPROTECT(2);
    return(ans);
}

SEXP which(SEXP x, Rboolean bool) {
    
    int i, j=0, n = length(x), *buf;
    SEXP ans;
    if (!isLogical(x)) error("Argument to 'which' must be logical");
    buf = (int *) R_alloc(n, sizeof(int));
    for (i = 0; i < n; i++) {
        if (LOGICAL(x)[i] == bool) {
            buf[j] = i + 1;
            j++;
        }
    }
    n = j;
    PROTECT(ans = allocVector(INTSXP, n));
    if (n) memcpy(INTEGER(ans), buf, sizeof(int) * n);
    
    UNPROTECT(1);
    return(ans);
}

// whichwrapper for R
SEXP whichwrapper(SEXP x, SEXP bool) {
    // if (LOGICAL(bool)[0] == NA_LOGICAL)
    //     error("bool should be logical TRUE/FALSE");
    return which(x, LOGICAL(bool)[0]);
}

extern SEXP char_integer64;
static union {
    double d;
    unsigned long long ull;
} u;

// internal version of anyNA for data.tables
SEXP anyNA(SEXP x, SEXP cols) {
    int i, j, n=0, this;
    double *dv;
    SEXP v, ans, class;
    
    if (!isNewList(x)) error("Internal error. Argument 'x' to CanyNA is type '%s' not 'list'", type2char(TYPEOF(x)));
    if (!isInteger(cols)) error("Internal error. Argument 'cols' to CanyNA is type '%s' not 'integer'", type2char(TYPEOF(cols)));
    for (i=0; i<LENGTH(cols); i++) {
        this = INTEGER(cols)[i];
        if (this<1 || this>LENGTH(x)) 
            error("Item %d of 'cols' is %d which is outside 1-based range [1,ncol(x)=%d]", i+1, this, LENGTH(x));
        if (!n) n = length(VECTOR_ELT(x, this-1));
    }
    ans = PROTECT(allocVector(LGLSXP, 1));
    LOGICAL(ans)[0]=0;
    for (i=0; i<LENGTH(cols); i++) {
        v = VECTOR_ELT(x, INTEGER(cols)[i]-1);
        if (!length(v) || isNewList(v) || isList(v)) continue; // like stats:::na.omit.data.frame, skip list/pairlist columns
        if (n != length(v))
            error("Column %d of input list x is length %d, inconsistent with first column of that item which is length %d.", i+1,length(v),n);
        j=0;
        switch (TYPEOF(v)) {
        case LGLSXP:
            while(j < n && LOGICAL(v)[j] != NA_LOGICAL) j++;
            if (j < n) LOGICAL(ans)[0] = 1;
            break;
        case INTSXP:
            while(j < n && INTEGER(v)[j] != NA_INTEGER) j++;
            if (j < n) LOGICAL(ans)[0] = 1;
            break;
        case STRSXP:
            while (j < n && STRING_ELT(v, j) != NA_STRING) j++;
            if (j < n) LOGICAL(ans)[0] = 1;
            break;
        case REALSXP:
            class = getAttrib(v, R_ClassSymbol);        
            if (isString(class) && STRING_ELT(class, 0) == char_integer64) {
                dv = (double *)REAL(v);
                for (j=0; j<n; j++) {
                    u.d = dv[j];
                    if (u.ull == NAINT64) {
                        LOGICAL(ans)[0] = 1;
                        break;
                    }
                }
            } else {
                while(j < n && !ISNAN(REAL(v)[j])) j++;
                if (j < n) LOGICAL(ans)[0] = 1;
            }
            break;
        case RAWSXP: 
            // no such thing as a raw NA
            // vector already initialised to all 0's
            break;
        case CPLXSXP:
            // taken from https://github.com/wch/r-source/blob/d75f39d532819ccc8251f93b8ab10d5b83aac89a/src/main/coerce.c
            while (j < n && !ISNAN(COMPLEX(v)[j].r) && !ISNAN(COMPLEX(v)[j].i)) j++;
            if (j < n) LOGICAL(ans)[0] = 1;
            break;
        default:
            error("Unknown column type '%s'", type2char(TYPEOF(v)));
        }
        if (LOGICAL(ans)[0]) break;
    }
    UNPROTECT(1);
    return(ans);
}

// equivalent of 'rowSums(is.na(dt) > 0L)' but much faster and memory efficient
SEXP dt_na(SEXP x, SEXP cols) {
    int i, j, n=0, this;
    double *dv;
    SEXP v, ans, class;
    
    if (!isNewList(x)) error("Internal error. Argument 'x' to Cdt_na is type '%s' not 'list'", type2char(TYPEOF(x)));
    if (!isInteger(cols)) error("Internal error. Argument 'cols' to Cdt_na is type '%s' not 'integer'", type2char(TYPEOF(cols)));
    for (i=0; i<LENGTH(cols); i++) {
        this = INTEGER(cols)[i];
        if (this<1 || this>LENGTH(x)) 
            error("Item %d of 'cols' is %d which is outside 1-based range [1,ncol(x)=%d]", i+1, this, LENGTH(x));
        if (!n) n = length(VECTOR_ELT(x, this-1));
    }
    ans = PROTECT(allocVector(LGLSXP, n));
    for (i=0; i<n; i++) LOGICAL(ans)[i]=0;
    for (i=0; i<LENGTH(cols); i++) {
        v = VECTOR_ELT(x, INTEGER(cols)[i]-1);
        if (!length(v) || isNewList(v) || isList(v)) continue; // like stats:::na.omit.data.frame, skip list/pairlist columns
        if (n != length(v))
            error("Column %d of input list x is length %d, inconsistent with first column of that item which is length %d.", i+1,length(v),n);
        switch (TYPEOF(v)) {
        case LGLSXP:
            for (j=0; j<n; j++) LOGICAL(ans)[j] |= (LOGICAL(v)[j] == NA_LOGICAL);
            break;
        case INTSXP:
            for (j=0; j<n; j++) LOGICAL(ans)[j] |= (INTEGER(v)[j] == NA_INTEGER);
            break;
        case STRSXP:
            for (j=0; j<n; j++) LOGICAL(ans)[j] |= (STRING_ELT(v, j) == NA_STRING);
            break;
        case REALSXP:
            class = getAttrib(v, R_ClassSymbol);        
            if (isString(class) && STRING_ELT(class, 0) == char_integer64) {
                dv = (double *)REAL(v);
                for (j=0; j<n; j++) {
                    u.d = dv[j];
                    LOGICAL(ans)[j] |= (u.ull == NAINT64);
                }
            } else {
                for (j=0; j<n; j++) LOGICAL(ans)[j] |= ISNAN(REAL(v)[j]);
            }
            break;
        case RAWSXP: 
            // no such thing as a raw NA
            // vector already initialised to all 0's
            break;
        case CPLXSXP:
            // taken from https://github.com/wch/r-source/blob/d75f39d532819ccc8251f93b8ab10d5b83aac89a/src/main/coerce.c
            for (j=0; j<n; j++) LOGICAL(ans)[j] |= (ISNAN(COMPLEX(v)[j].r) || ISNAN(COMPLEX(v)[j].i));
            break;
        default:
            error("Unknown column type '%s'", type2char(TYPEOF(v)));
        }
    }
    UNPROTECT(1);
    return(ans);
}