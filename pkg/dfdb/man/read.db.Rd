\name{read.db}
\alias{read.db}

\title{
Read a Data File into an SQLite Data Table.
}
\description{
Read data from a delimited data file into a SQLite data table using the \code{\link{read.table}} function on ``chunks'' of the input file.
}

\usage{
read.db(file, dsname, database=paste(dsname, ".db", sep = ""), header = TRUE, ...)
}

\arguments{
  \item{file}{quoted name of the file in which the data reside. The data should be in a form acceptable to \code{\link{dbWriteTable}}.}
  \item{dsname}{quoted name of the data table to be created.}
  \item{database}{quoted name of the database file in which the data table is to be stored; if missing, the a file named \code{dsname.db} (where ``\code{dsname}'' is the \code{dsname} argument) will be created in the current directory and the data table will be written there.}
  \item{header}{does the first row of the data file contain variable names (default, \code{TRUE})?}
  \item{\dots}{argument to be passed to \code{\link{dbWriteTable}}.}
}

\value{
\code{TRUE} if the operation succeeded; \code{FALSE} if it failed.
}

\author{
John Fox, \email{jfor@mcmaster.ca}.
}


\seealso{
\code{\link{dbWriteTable}}, \code{\link{sqliteImportFile}}, \code{\link{SQLiteDataset}}
}

\examples{

}

\keyword{ manip }
