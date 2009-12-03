# create a pseudo-dataframe (SQLiteDataset object)

# last modified 3 December 2009

SQLiteDataset <- function(table, database=paste(table, ".db", sep=""), 
		stringsAsFactors = default.stringsAsFactors()){
	dr <- dbDriver("SQLite")
	conn <- dbConnect(dr, dbname = database)
    if (length(table) != 1 || !is.character(table)) stop("table must be a data-base table name")
    if (!dbExistsTable(conn, table)) stop('table "', table, '" does not exist')
    # read one row
    res <- dbSendQuery(conn, paste("select * from", table))
    first.row <- fetch(res, n=1)
    dbClearResult(res)
    col.names <- colnames(first.row)
    col.classes <- sapply(first.row, class)
	row.names <- if (col.names[1] == "row_names"){
		col.names <- col.names[-1]
		TRUE
	}
	else FALSE
#    if (isTRUE(rownames)){
#        rownames <- which(col.classes == "character")
#        if (length(rownames) == 0) row.names <- "" 
#		else{
#                row.names <- col.names[rownames[1]]
#                col.names <- col.names[-rownames[1]]
#                col.classes <- col.classes[-rownames[1]]
#			}
#        }
#	else if (is.character(rownames) || is.numeric(rownames)){
#		if (is.character(rownames)) rownames <- which(col.names == rownames)
#		row.names <- col.names[rownames]
#		col.names <- col.names[-rownames]
#		col.classes <- col.classes[-rownames]
#	}
#	else row.names <- ""
    if (stringsAsFactors) col.classes[col.classes == "character"] <- "factor"
    result <- list(conn=conn, table=table, database=database, row.name=row.names, col.names=col.names, col.classes=col.classes)
    class(result) <- c("SQLiteDataset", "data.frame")
    result
    }
    
print.SQLiteDataset <- function(x, ..., verbose=FALSE){
    if (verbose) {
        x <- x[]
        NextMethod("print")
        }
    else {
		cat(  "database:          ", database(x))
        cat("\ntable:             ", table.name(x))
        cat("\nrow names:         ", row.name(x))
        dim <- dim(x)
        cat("\nnumber of rows:    ", dim[1])
        cat("\nnumber of columns: ", dim[2], "\n")
        invisible(x)
        }
    }
    
summary.SQLiteDataset <- function(object, ..., rows){
    object <- if (missing(rows)) object[]
        else object[rows, ]
    NextMethod("summary")
    }

close.SQLiteDataset <- function(con, ...){
	dbDisconnect(connection(con))
}