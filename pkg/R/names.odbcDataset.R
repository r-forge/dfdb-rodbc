# last modified 28 November 2008 by J. Fox 

names.odbcDataset <-
function(x){
    # returns names of odbcDataset object
    x <- unclass(x)
    names <- sqlColumns(x$channel, x$table)[[x$column.selector]]
    if (x$rownames) names[-1] else names
    }

