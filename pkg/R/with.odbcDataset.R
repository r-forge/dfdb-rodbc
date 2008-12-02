# last modified 28 November 2008 by J. Fox 

with.odbcDataset <-
function(data, expr, rows,  ...){
    vars <- all.vars(substitute(expr))
    cols <- vars[vars %in% names(data)]
    Data <- if (missing(rows)) data[, cols, drop=FALSE]
        else data[rows, cols, drop=FALSE] 
    eval(substitute(expr), Data)
    }

