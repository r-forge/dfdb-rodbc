\name{SQLiteDataset}
\alias{SQLiteDataset}
\alias{print.SQLiteDataset}
\alias{summary.SQLiteDataset}

\title{
Create an SQLiteDataset Object
}

\description{
Create an SQLiteDataset pseudo-data-frame object.
}

\usage{
SQLiteDataset(conn, table, rownames, stringsAsFactors=default.stringsAsFactors())
\method{print}{SQLiteDataset}(x, ..., verbose=FALSE)
\method{summary}{SQLiteDataset}(object, ..., rows)
}

\arguments{
  \item{conn}{a connection to a SQLite database.}
  \item{table}{the quoted name of a data table in the database.}
  \item{rownames}{\code{TRUE} if the first character variable in the data base contains row names; \code{FALSE} if there is no row names variable. If not specified, the function will look for a suitable variable in the data base.}
  \item{stringsAsFactors}{if \code{TRUE} (the default), character variables are treated as factors; the conversion is done when data are retrieved from the data table.}
  \item{x, object}{an object of class \code{SQLiteDataSet}.}
  \item{verbose}{if \code{TRUE} (the default is \code{FALSE}), the \emph{whole} data set is retrieved and printed; otherwise just some information about the data set is printed.}
  \item{rows}{a quoted SQL query to select rows from the data set; if missing, then all rows are retrieved.}
  \item{\dots}{arguments to be passed down.}
}

\details{
The function \code{SQLiteDataset} creates a pseudo-data-frame, which then can be used more or less like a data frame.
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
Arrests <- SQLiteDataset(con, "USArrests")
Arrests
summary(Arrests)
summary(Arrests, rows="Murder > 10")
dbDisconnect(con)
}

\keyword{ manip1 }
