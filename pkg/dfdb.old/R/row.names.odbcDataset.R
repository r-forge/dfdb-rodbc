row.names.odbcDataset <-
function(x) {
    # returns row names of odbcDataset object
    x <- unclass(x)
    if (x$rownames) {
        name <- sqlColumns(x$channel, x$table)[[x$column.selector]][1] 
        as.character(sqlQuery(x$channel, paste("select", name, "from", x$table))[[1]])
        }
    else NULL
    }

