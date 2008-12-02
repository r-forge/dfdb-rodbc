# last modified 28 November 2008 by J. Fox 

dim.odbcDataset <-
function (x) {
    # returns dimensions of odbcDataset object
    if (!is.null(rownames(x))) c(length(row.names(x)), length(names(x)))
    else c(length(x[[1]]), length(names(x)))
    }

