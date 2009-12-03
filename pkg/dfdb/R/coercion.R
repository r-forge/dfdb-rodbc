# coercion methods for SQLiteDatatset objects

as.data.frame.SQLiteDataset <- function(x, row.names, optional, ...){
    x[]
}

as.list.SQLiteDataset <- function(x, ...) {
    as.list(as.data.frame(x))
}

as.matrix.SQLiteDataset <- function(x, ...) {
    as.matrix(as.data.frame(x))
}
