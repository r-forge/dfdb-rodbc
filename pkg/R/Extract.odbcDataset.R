# last modified 28 November 2008 by J. Fox 

"[[.odbcDataset" <-
	function(x, i){
	# indexing method for odbcDataset objects
	x <- unclass(x)
	if (is.numeric(i)) i <- sqlColumns(x$channel, x$table)[[x$column.selector]][i + x$rownames] 
	sqlQuery(x$channel, paste("select", i, "from", x$table))[[1]]
}

"[.odbcDataset" <-
	function(x, i, j, drop=TRUE, ...){
	# indexing method for odbcDataset objects: supports SQL fetches
	#   i: row 'subscript' is a quoted SQL statement
	#   j: column subscript, may be column numbers, negative numbers, or names
	same.sign <- function(x) {
		any(x > 0) == all(x >= 0)
	}
	x <- unclass(x) 
	names <- sqlColumns(x$channel, x$table)[[x$column.selector]]
	selection <- if (missing(j)) "*" 
		else if (is.numeric(j)) {
			j <- j[j != 0]
			if (length(j) == 0) return(NULL)
			if (!same.sign(j)) stop("cannot mix positive and negative subscripts")
			if (j[1] > 0) names[j + x$rownames]
			else names[-1 * c(rep(1, x$rownames), (abs(j) + x$rownames))]
		}                        
		else j
	selection <- if (selection == "*" || !x$rownames) selection
		else c(names[1], selection)
	if (selection != "*" && (any(is.na(selection)) || any(!is.element(selection, names)))) stop("bad column index")
	result <- if (missing(i)) sqlQuery(x$channel, paste("select", paste(selection, collapse=","), "from", x$table))
		else {
			if (!is.character(i) && length(i) != 1) stop("row 'subscript' must be an SQL row selector")
			sqlQuery(x$channel, paste("select", paste(selection, collapse=","), "from", x$table, "where", i))
		}
	if (x$rownames) {
		rownames(result) <- as.character(result[,1])
		result <- result[, -1, drop=drop]
	}
	if (drop && length(dim(result)) == 2 && dim(result)[2] == 1) result[,,drop=TRUE] else result
}

"$.odbcDataset" <-
	function(x, i){
	# indexing method for odbcDataset objects
	x <- unclass(x)
	sqlQuery(x$channel, paste("select", i, "from", x$table))[[1]]
}


