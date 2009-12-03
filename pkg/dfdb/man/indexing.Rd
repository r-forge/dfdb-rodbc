\name{indexing.SQLiteDataset}
\alias{[[.SQLiteDataset}
\alias{[.SQLiteDataset}
\alias{$.SQLiteDataset}

\title{
Indexing SQLiteDataset Objects
}
\description{
Methods for indexing SQLiteDataset Objects. The \code{[} method can take a quoted SQL query as its ``row'' argument.
}
\usage{
\method{[[}{SQLiteDataset}(x, i)
\method{[}{SQLiteDataset}(x, i, j, drop = TRUE, ...)
\method{$}{SQLiteDataset}(x, i)
}

\arguments{
  \item{x}{an SQLiteDataset object.}
  \item{i}{for \code{[[} and \code{$}, a column name or number; for \code{[}, a quoted SQL query to select rows from the data set.}
  \item{j}{a vector of quoted column names or of column numbers.}
  \item{drop}{if \code{TRUE} (the default) and only one column is selected, return a vector.}
  \item{\dots}{ignored.}
}

\note{
An empty subscript returns all rows or columns; to select the entire data set, specify \code{dsname[]}.
}

\value{
a data frame or vector.
}

\author{
John Fox, \email{jfor@mcmaster.ca}.
}


\seealso{
\code{\link{SQLiteDataset}}, \code{\link[base]{$}}
}

\examples{
dr <- dbDriver("SQLite")
tfile <- tempfile()
con <- dbConnect(dr, dbname = tfile)
dbWriteTable(con, "USArrests", USArrests)
dbDisconnect(con)
Arrests <- SQLiteDataset("USArrests", database=tfile)
Arrests$Murder
Arrests[["Murder"]]
Arrests["Murder > 10", c("Murder", "Rape")]
close(Arrests)
}

\keyword{ manip }
