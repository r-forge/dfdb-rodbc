\name{utilities.SQLiteDataset}
\alias{col.classes}
\alias{col.classes.SQLiteDataset}
\alias{col.names}
\alias{col.names.SQLiteDataset}
\alias{connection}
\alias{connection.SQLiteDataset}
\alias{dim.SQLiteDataset}
\alias{dimnames.SQLiteDataset}
\alias{names.SQLiteDataset}
\alias{row.name}
\alias{row.name.SQLiteDataset}
\alias{row.names.SQLiteDataset}
\alias{table.name}
\alias{table.name.SQLiteDataset}
\alias{with.SQLiteDataset}

\title{
SQLiteDataset Utilities
}
\description{
Utility functions for RSQLiteDataset object.
}
\usage{
col.classes(dataset, ...)
\method{col.classes}{SQLiteDataset}(dataset, ...)
col.names(dataset, ...)
\method{col.names}{SQLiteDataset}(dataset, ...) 
connection(dataset, ...)
\method{connection}{SQLiteDataset}(dataset, ...)
\method{dim}{SQLiteDataset}(x)
\method{dimnames}{SQLiteDataset}(x)
\method{names}{SQLiteDataset}(x)
row.name(dataset, ...)
\method{row.name}{SQLiteDataset}(dataset, ...)
\method{row.names}{SQLiteDataset}(x)
table.name(dataset, ...)
\method{table.name}{SQLiteDataset}(dataset, ...)
\method{with}{SQLiteDataset}(data, expr, rows,  ...)
}

\arguments{
  \item{data, dataset, x}{SQLiteDataset object.}
  \item{expr}{a possibly multiline \R{} expression to evaluate within the environment constructed from the data set.}
  \item{rows}{an optional quoted SQL query defining the rows to retrieve; if missing, all rows are retrieved.}
  \item{\dots}{ignored.}
}

\details{
Most of these functions report information about a SQLiteDataset object, and are self-explanatory. 
}

\value{
Varies with the function.
}

\author{
John Fox, \email{jfor@mcmaster.ca}.
}


\seealso{
\code{\link{SQLiteDataset}}, \code{\link{dim}}, \code{\link{dimnames}}, \code{\link{names}}, \code{\link{rownames}}, \code{\link{with}}
}

\examples{
dr <- dbDriver("SQLite")
tfile <- tempfile()
con <- dbConnect(dr, dbname = tfile)
dbWriteTable(con, "USArrests", USArrests)
Arrests <- SQLiteDataset(con, "USArrests")
col.classes(Arrests)
col.names(Arrests)
connection(Arrests)
dim(Arrests)
dimnames(Arrests)
names(Arrests)
row.name(Arrests)
row.names(Arrests)
table.name(Arrests)
with(Arrests, mean(Rape))
with(Arrests, mean(Rape), rows="Murder > 10")
dbDisconnect(con)
}

\keyword{ manip }
