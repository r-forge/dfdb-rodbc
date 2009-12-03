# indexing methods for SQLiteDatatset objects

`$.SQLiteDataset` <- function(x, i){
    if (length(i) != 1) stop("index must be 1 element")
    col.names <- col.names(x)
    if (is.numeric(i)) i <- col.names[i]
    if (! i %in% col.names) stop(i, " is not in the data set")
    res <- dbGetQuery(connection(x), paste("select", i, "from", table.name(x)))[[1]]
    class <- col.classes(x)[i]
    if (class == "factor") as.factor(res) else res
}

`[[.SQLiteDataset` <- function(x, i){
    if (length(i) != 1) stop("index must be 1 element")
    col.names <- col.names(x)
    if (is.numeric(i)) i <- col.names[i]
    if (! i %in% col.names) stop(i, " is not in the data set")
    res <- dbGetQuery(connection(x), paste("select", i, "from", table.name(x)))[[1]]
    class <- col.classes(x)[i]
    if (class == "factor") as.factor(res) else res
}


`[.SQLiteDataset` <- function(x, i, j, drop=TRUE, ...){
    #   i: row 'subscript' is a quoted SQL statement
    #   j: column subscript, may be column numbers, negative numbers, or names
    same.sign <- function(x) {
        any(x > 0) == all(x >= 0)
    }
    col.names <- col.names(x)
    selection <- if (missing(j)) "*" 
            else if (is.numeric(j)) {
                j <- j[j != 0]
                if (length(j) == 0) return(NULL)
                if (!same.sign(j)) stop("cannot mix positive and negative subscripts")
                col.names[j]
            }                        
            else j
    if (selection != "*" && (any(is.na(selection)) || any(!is.element(selection, col.names)))) 
        stop("bad column index")
    row.name <- row.name(x)
    selection <- if (selection == "*" || row.name == "") selection
            else c(row.name, selection)
    result <- if (missing(i)) dbGetQuery(connection(x), 
                        paste("select", paste(selection, collapse=","), "from", table.name(x)))
            else {
                if (!is.character(i) && length(i) != 1) 
                    stop("row 'subscript' must be an SQL row selector")
                dbGetQuery(connection(x), paste("select", 
                                paste(selection, collapse=","), "from", table.name(x), "where", i))
            }
    if (row.name != "") {
        rownames(result) <- as.character(result[, 1])
        result <- result[, -1, drop=FALSE]
    }
    colnames <- colnames(result)
    result <- if (drop && length(dim(result)) == 2 && dim(result)[2] == 1) 
                result[, , drop=TRUE] 
            else result
    factors <- colnames[col.classes(x)[colnames] == "factor"]
    if (length(factors > 0)){
        if (length(colnames) > 1)
        for (factor in factors) result[[factor]] <- as.factor(result[[factor]])
        else result <- (as.factor(result))
    }
    result
}
