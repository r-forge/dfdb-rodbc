# read a delimited file into a SQLite data table

read.db <- function(file, dsname, database=paste(dsname, ".db", sep=""), 
    header=TRUE, chunk.size, initial=100, colClasses, col.names, overwrite=FALSE, ...){
    # file: file to read
    # dsname: name of dataset (table) to create
    # database: database file within which to store the dataset
    # header: first line contains variable names?
    # chunk.size: number of rows to read at a time
    # initial: number of rows to read initially
    # colClasses: as in read.table(); if missing, taken from first chunk
    # col.names: as in read.table; if missing, taken from first chunk
    # overwrite: overwrite if dsname exists in database?
    # ...: pass to read.table()
    dr <- dbDriver("SQLite")
    con <- dbConnect(dr, dbname = database)
    on.exit(dbDisconnect(con))
    chunk <- if (missing(colClasses) && missing(col.names))
                read.table(file, header=header, nrows=initial, ...)
            else if (missing(colClasses)) 
                read.table(file, header=header, nrows=initial, col.names=col.names, ...)
            else if (missing(col.names))
                read.table(file, header=header, nrows=initial, colClasses=colClasses, ...)
            else read.table(file, header=header, nrows=initial, col.names=col.names, colClasses=colClasses, ...)
    if (missing(col.names)) col.names <- colnames(chunk)
    col.names <- make.db.names(dr, col.names)
    colnames(chunk) <- col.names
    if (!sqliteWriteTable(con, dsname, chunk, row.names = TRUE, overwrite=overwrite))
        stop("SQL write error")
    if (missing(colClasses)) colClasses <- sapply(chunk, "class")
    rownames <- rownames(chunk)
    numeric.rownames <- all(rownames == seq(along=rownames))
    if (nrow(chunk) < initial){
        cat(paste(nrow(chunk), "records read.\n"))
        return(invisible(NULL))
        }
    if (missing(chunk.size)){
        initial.size <- object.size(chunk)/1e6
        remaining.memory <- memory.limit() - memory.size()
        multiple <- remaining.memory/(4*initial.size)
        chunk.size <- round(multiple*initial)
    }
    last.row <- initial
	i <- 0
    repeat {
        chunk <- read.table(file, header=FALSE, nrows=chunk.size, skip=last.row + header, 
            colClasses=colClasses, col.names=col.names, ...)
        nrow <- nrow(chunk)
        if (nrow == 0) break
        if (numeric.rownames) rownames(chunk) <- (last.row + 1):(last.row + nrow)
        if (!sqliteWriteTable(con, dsname, chunk, row.names = TRUE, append = TRUE))
            stop("SQL write error")
        last.row <- last.row + nrow
        if (nrow < chunk.size) break
		remove(chunk)
		gc(verbose=TRUE)
		i <- i + 1
		cat(paste(" ", i))
    }
    cat(paste(last.row, "records read.\n"))
    invisible(NULL)
}
