# utility functions and methods for SQLiteDataset objects

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
