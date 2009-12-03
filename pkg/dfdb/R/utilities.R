# utility functions and methods for SQLiteDataset objects

# last modified 3 December 2009 by J. Fox

col.classes <- function(dataset, ...) UseMethod("col.classes")

col.classes.SQLiteDataset <- function(dataset, ...) {
    dataset <- unclass(dataset)
    dataset$col.classes
}

col.names <- function(dataset, ...) UseMethod("col.names")

col.names.SQLiteDataset <- function(dataset, ...) {
    dataset <- unclass(dataset)
    dataset$col.names
}

connection <- function(dataset, ...) UseMethod("connection")

connection.SQLiteDataset <- function(dataset, ...) {
    dataset <- unclass(dataset)
    dataset$conn
}

dim.SQLiteDataset <- function(x) {
    nrow <- dbGetQuery(connection(x), paste("select count(*) from", table.name(x)))[[1, 1]]
    c(nrow, length(names(x)))
}

dimnames.SQLiteDataset <- function(x) list(row.names(x), names(x))

names.SQLiteDataset <- function(x){
    col.names(x)
}

row.name <- function(dataset, ...) UseMethod("row.name")

row.name.SQLiteDataset <- function(dataset, ...) {
    dataset <- unclass(dataset)
    dataset$row.name
}

row.names.SQLiteDataset <- function(x){
    row.name <- row.name(x)
    if (row.name != "")
        dbGetQuery(connection(x), paste("select", row.name, "from", table.name(x)))[,1]
    else as.character(seq(length=nrow(x)))}

table.name <- function(dataset, ...) UseMethod("table.name")

table.name.SQLiteDataset <- function(dataset, ...) {
    dataset <- unclass(dataset)
    dataset$table
}

with.SQLiteDataset <- function(data, expr, rows,  ...){
    vars <- all.vars(substitute(expr))
    cols <- vars[vars %in% names(data)]
    Data <- if (missing(rows)) data[, cols, drop=FALSE]
        else data[rows, cols, drop=FALSE] 
    eval(substitute(expr), Data)
}

within.SQLiteDataset <- function(data, expr, rows,  ...){
	row.name <- row.name(data)
	if (row.name == "") stop("within only works when there is a row-name column in the data table")
	con <- connection(data)
	vars <- all.vars(substitute(expr))
	cols <- vars[vars %in% names(data)]
	Data <- if (missing(rows)) data[, cols, drop=FALSE]
			else data[rows, cols, drop=FALSE]
	old.names <- colnames(Data)         
	# next few lines adapted from within.data.frame()
	env <- evalq(environment(), Data)
	eval(substitute(expr), env)
	lst <- as.list(env)
	lst <- lst[!sapply(lst, is.null)]
	nD <- length(del <-setdiff(old.names, (nl <- names(lst))))
	Data[nl] <- lst
	if (nD) Data[del] <- if (nD == 1) NULL else vector("list", nD) 
	# end of adapted lines
	for (var in old.names) Data[var] <- NULL
	original.table <- table.name(data)
	temp.table <- paste(original.table, "_temp", sep="")
	command <- paste("CREATE INDEX _ORIGINAL_INDEX ON", original.table, "(", row.name, ")")
	dbGetQuery(con, command)
#	con <- connection(data)
#	dbGetQuery(con, "PRAGMA ASYNCHRONOUS=0")
	new.names <- colnames(Data)
	Data[paste(row.name, "_2", sep="")] <- rownames(Data)
	dbWriteTable(con, temp.table, Data, row.names=FALSE, overwrite=TRUE)
	command <- paste("CREATE INDEX _TEMP_INDEX ON ", temp.table, " (", row.name, "_2)", sep="")
	dbGetQuery(con, command)
	command <- paste("CREATE TABLE ", temp.table, "_2 AS SELECT ",
			paste(c(row.name, colnames(data), new.names), collapse=","),
			" FROM ", original.table, " LEFT OUTER JOIN ",
			temp.table, " ON ", original.table, ".", row.name," = ", temp.table, ".", row.name,"_2;", sep="")
	dbGetQuery(con, command)
	backup.table <- paste(original.table, "_backup", sep="")
	if (dbExistsTable(con, backup.table)) dbGetQuery(con, paste("DROP TABLE", backup.table))
	dbGetQuery(con, "DROP INDEX _ORIGINAL_INDEX")
	dbGetQuery(con, "DROP INDEX _TEMP_INDEX")
	dbGetQuery(con, paste("ALTER TABLE", original.table, "RENAME TO", backup.table))
	dbGetQuery(con, paste("DROP TABLE", temp.table))
	dbGetQuery(con, paste("ALTER TABLE ", temp.table, "_2 RENAME TO ", original.table, sep=""))
	assign(deparse(substitute(data)), SQLiteDataset(con, original.table), envir=parent.frame())
}
