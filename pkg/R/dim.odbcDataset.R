# last modified 22 Dec 2008 by OGC

dim.odbcDataset <-
function (x) {

    #returns dimensions of odbcDataset object

    if (unclass(x)$rownames) c(length(row.names(x)), length(names(x)))
    else c(length(x[[1]]), length(names(x)))

    }

