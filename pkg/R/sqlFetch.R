# last modified 28 November 2008 by J. Fox 

sqlFetch <-
function (channel, sqtable, ..., colnames = FALSE, rownames = FALSE) 
{ # slightly modified from the RODBC package
    if (channel < 0) 
        stop("invalid channel")
    if (missing(sqtable)) 
        stop("Missing parameter")
    tablename <- as.character(substitute(sqtable))                 # modified
    tablename <- if (exists(tablename) && is.character(sqtable))   # lines
        sqtable else tablename                                     #
    dbname <- RODBC:::odbcTableExists(channel, tablename)
    ans <- sqlQuery(channel, paste("SELECT * FROM", dbname), 
        ...)
    if (is.logical(colnames) && colnames) {
        colnames(ans) <- as.character(as.matrix(ans[1, ]))
        ans <- ans[-1, ]
    }
    if (is.logical(rownames) && rownames) {
        rownames(ans) <- as.character(as.matrix(ans[, 1]))
        ans <- ans[, -1]
    }
    ans
}

