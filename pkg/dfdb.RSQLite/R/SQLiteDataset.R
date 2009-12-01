# create a pseudo-dataframe (SQLiteDataset object)

SQLiteDataset <- function(conn, table, rownames, stringsAsFactors = default.stringsAsFactors()){
    # arguments:
    #   conn: connection returned by dbConnect()
    #   table: name of database table
    #   rownames: TRUE if the first character variable in the data base contains row names;
    #             FALSE if there is no row names variable;
    #             if not specified, the function will look for a suitable variable in the
    #             data base
    if (class(conn) != "SQLiteConnection") stop("conn must be an SQLite connection")
    if (length(table) != 1 || !is.character(table)) stop("table must be a data-base table name")
    if (!dbExistsTable(conn, table)) stop('table "', table, '" does not exist')
    # read one row
    res <- dbSendQuery(conn, paste("select * from", table))
    first.row <- fetch(res, n=1)
    dbClearResult(res)
    col.names <- colnames(first.row)
    col.classes <- sapply(first.row, class)
    if (missing(rownames)){
        rownames <- which(col.classes == "character")
        if (length(rownames) == 0) "" else 
                row.names <- col.names[rownames[1]]
                col.names <- col.names[-rownames[1]]
                col.classes <- col.classes[-rownames[1]]
        }
    if (stringsAsFactors) col.classes[col.classes == "character"] <- "factor"
    result <- list(conn=conn, table=table, row.name=row.names, col.names=col.names, col.classes=col.classes)
    class(result) <- c("SQLiteDataset", "data.frame")
    result
    }
    
print.SQLiteDataset <- function(x, ..., verbose=FALSE){
    if (verbose) {
        x <- x[]
        NextMethod("print")
        }
    else {
        cat(  "table:              ", table.name(x))
        cat("\nrow names variable: ", row.name(x))
        dim <- dim(x)
        cat("\nnumber of rows:     ", dim[1])
        cat("\nnumber of columns:  ", dim[2], "\n")
        invisible(x)
        }
    }
    
summary.SQLiteDataset <-
function(object, ..., rows){
    object <- if (missing(rows)) object[]
        else object[rows, ]
    NextMethod("summary")
    }
