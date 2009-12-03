\name{read.db}
\alias{read.db}

\title{
Read a Data File into an SQLite Data Table.
}
\description{
Read data from a delimited data file into a SQLite data table using the \code{\link{read.table}} function on ``chunks'' of the input file.
}

\usage{
read.db(file, dsname, database=paste(dsname, ".db", sep = ""), header = TRUE, 
    chunk.size, initial = 100, colClasses, col.names, overwrite = FALSE, ...)
}

\arguments{
  \item{file}{quoted name of the file in which the data reside. The data should be in a form acceptable to \code{\link{read.table}}.}
  \item{dsname}{quoted name of the data table to be created.}
  \item{database}{quoted name of the database file in which the data table is to be stored; if missing, the a file named \code{dsname.db} will be created in the current directory and the data table will be written there.}
  \item{header}{does the first row of the data file contain variable names (default, \code{TRUE})?}
  \item{chunk.size}{number of rows to read at a time; if missing, the chunk size will be set so that each chunk fits comfortably in the available memory.}
  \item{initial}{number of rows to read initially, to determine the data classes of the columns if these aren't given explicitly; the default is \code{100}.}
  \item{colClasses}{data classes of the columns; if missing, will be determined from the initial chunk.}
  \item{col.names}{names for the columns (optional); will be coerced to legal SQLite names (e.g., periods changed to underscores).}
  \item{overwrite}{if table \code{dsname} alread exists in the database file (default, \code{FALSE})?}
  \item{\dots}{argument to be passed to \code{\link{read.table}}.}
}

\value{
Used for its side effect: to create a data table in a database file.
}

\author{
John Fox, \email{jfor@mcmaster.ca}.
}


\seealso{
\code{\link{read.table}}, \code{\link{SQLiteDataset}}
}

\examples{

}

\keyword{ manip }
