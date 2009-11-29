# last modified 28 November 2008 by J. Fox 

summary.odbcDataset <-
function(object, ...){
    # summary method for odbcDataset objects
    object <- unclass(object)
    object <- sqlFetch(object[["channel"]], object[["table"]], rownames=object[["rownames"]])
    NextMethod("summary")
    }

