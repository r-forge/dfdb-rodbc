# last modified 28 November 2008 by J. Fox 

odbcDataset <-
function(channel, table, rownames){
    # create a pseudo-dataframe (odbcDataset object)
    # arguments:
    #   channel: object returned by odbcConnect()
    #   table: name of database table
    #   rownames: TRUE if the first character variable in the data base contains row names;
    #             FALSE if there is no row names variable;
    #             if not specified, the function will look for a suitable variable in the
    #             data base
    if (length(channel) != 1 || !is.integer(channel) || channel < 0) stop("channel must be a non-negative integer")
    if (length(table) != 1 || !is.character(table)) stop("table must be a data-base table name")
    RODBC:::odbcTableExists(channel, table)
    col.info <- sqlColumns(channel, table)
    if (!is.data.frame(col.info))
        stop(paste("database connected on channel", channel, "not supported\ndatabase info:", odbcGetInfo(channel)))
    col.sel <- which(toupper(names(sqlColumns(channel, table))) == "COLUMN_NAME")
    if (1 != length(col.sel)) 
        stop(paste("database connected on channel", channel, "not supported\ndatabase info:", odbcGetInfo(channel)))
    if (missing(rownames)){
        type.sel <- which(toupper(names(sqlColumns(channel, table))) == "TYPE_NAME")
        if (1 != length(type.sel)) 
            stop(paste("database connected on channel", channel, "not supported\ndatabase info:", odbcGetInfo(channel)))
        rownames <- 1 == length(grep("CHAR", toupper(col.info[1, type.sel])))
        }
    result <- list(channel=channel, table=table, rownames=rownames, column.selector=col.sel)
    class(result) <- c("odbcDataset", "data.frame")
    result
    }

