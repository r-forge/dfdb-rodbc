# last modified 28 November 2008 by J. Fox 

as.data.frame.odbcDataset <-
function(x, row.names, optional, ...){
    # returns odbcDataset object as a dataframe
    x <- unclass(x)
    sqlFetch(x$channel, x$table, rownames=x$rownames)
    }

