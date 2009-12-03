\name{SQLiteDataset}
\alias{SQLiteDataset}
\alias{close.SQLiteDataset}
\alias{print.SQLiteDataset}
\alias{summary.SQLiteDataset}

\title{
Create an SQLiteDataset Object
}

\description{
Create an SQLiteDataset pseudo-data-frame object.
}

\usage{
SQLiteDataset(table, database=paste(table, ".db", sep=""),
	stringsAsFactors=default.stringsAsFactors())
\method{print}{SQLiteDataset}(x, ..., verbose=FALSE)
\method{summary}{SQLiteDataset}(object, ..., rows)
\method{close}{SQLiteDataset}(con, ...)
}

\arguments{
  \item{table}{the quoted name of a data table in the database.}
  \item{database}{quoted name of the database file in which the data table is located; if missing, a file named \code{table.db} (where ``\code{table}'' is the \code{table} argument) in the current directory will be assumed.}
  \item{stringsAsFactors}{if \code{TRUE} (the default), character variables are treated as factors; the conversion is done when data are retrieved from the data table.}
  \item{x, object, con}{an object of class \code{SQLiteDataSet}.}
  \item{verbose}{if \code{TRUE} (the default is \code{FALSE}), the \emph{whole} data set is retrieved and printed; otherwise just some information about the data set is printed.}
  \item{rows}{a quoted SQL query to select rows from the data set; if missing, then all rows are retrieved.}
  \item{\dots}{arguments to be passed down.}
}

\details{
The function \code{SQLiteDataset} creates a pseudo-data-frame, which then can be used more or less like a data frame. Use \code{close} to close the connection to the database containing the data table referenced by the object.
}

\value{
an object of class \code{SQLiteDataset}.
}

\author{
John Fox, \email{jfor@mcmaster.ca}.
}

\seealso{
\code{\link{read.db}}, \code{\link{dbDriver}}, \code{\link{dbConnect}}, 
\code{\link{dbDisconnect}}, \code{\link{$.SQLiteDataset}}, 
\code{\link{as.data.frame.SQLiteDataset}}
}

\examples{
dr <- dbDriver("SQLite")
tfile <- tempfile()
con <- dbConnect(dr, dbname = tfile)
dbWriteTable(con, "USArrests", USArrests)
dbDisconnect(con)
Arrests <- SQLiteDataset("USArrests", database=tfile)
Arrests
summary(Arrests)
summary(Arrests, rows="Murder > 10")
close(Arrests)
}

\keyword{ manip1 }
