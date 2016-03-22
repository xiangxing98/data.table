which_ <- function(x, bool = TRUE) {
    # fix for #1467, quotes result in "not resolved in current namespace" error
    .Call(Cwhichwrapper, x, bool)
}

# Equivalent of 'rowSums(is.na(dt) > 0L)' but much faster and memory efficient.
# Also called "complete.cases" in base. Unfortunately it's not a S3 generic.
# Also handles bit64::integer64. TODO: export this?
# For internal use only. 'by' requires integer input. No argument checks here yet.
is_na <- function(x, by=seq_along(x)) .Call(Cdt_na, x, by)
any_na <- function(x, by=seq_along(x)) .Call(CanyNA, x, by)