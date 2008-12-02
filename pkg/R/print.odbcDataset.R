# last modified 28 November 2008 by J. Fox 

print.odbcDataset <-
function(x, ..., verbose=FALSE){
    # print method for odbcDataset objects
    x <- unclass(x)  # necessary because of $.odbcDataset
    if (verbose) {
        x <- sqlFetch(x$channel, x$table, rownames=x$rownames)
        NextMethod("print")
        }
    else {
        cat(  "channel:           ", x$channel)
        cat("\ntable:             ", x$table)
        cat("\nrow names:         ", x$rownames)
        class(x) <- c("odbcDataset", "data.frame")
        dim <- dim(x)
        cat("\nnumber of rows:    ", dim[1])
        cat("\nnumber of columns: ", dim[2], "\n")
        invisible(x)
        }
    }

