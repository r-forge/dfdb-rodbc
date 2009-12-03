\name{coercion.SQLiteDataset}
\alias{as.data.frame.SQLiteDataset}
\alias{as.list.SQLiteDataset}
\alias{as.matrix.SQLiteDataset}

\title{
Coerce SQLiteDataset Objects to Other Classes
}

\description{
Methods to coercise a SQLiteDataset object to a data frame, list, or matrix.
}

\usage{
\method{as.data.frame}{SQLiteDataset}(x, row.names, optional, ...)
\method{as.list}{SQLiteDataset}(x, ...)
\method{as.matrix}{SQLiteDataset}(x, ...)
}

\arguments{
  \item{x}{an SQLiteDataset object.}
  \item{row.names}{not used.}
  \item{optional}{not used.}
  \item{\dots}{not used.}
}

\value{
a data frame, list, or matrix containing the entire data set.
}

\author{
John Fox, \email{jfor@mcmaster.ca}.
}


\seealso{
\code{\link{SQLiteDataset}}, \code{\link{as.data.frame}}, \code{\link{as.list}}, \code{\link{as.matrix}}
}

\examples{
dr <- dbDriver("SQLite")
tfile <- tempfile()
con <- dbConnect(dr, dbname = tfile)
dbWriteTable(con, "USArrests", USArrests)
dbDisconnect(con)
Arrests <- SQLiteDataset("USArrests", database=tfile)
as.data.frame(Arrests)
as.list(Arrests)
as.matrix(Arrests)
close(Arrests)
}

\keyword{ manip }
