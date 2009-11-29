\name{with.odbcDataset}
\alias{with.odbcDataset}

\title{Evaluate an R Expression Using Data from an odbcDataset Object}
\description{
  Evaluate an R expression using data from an odbcDataset object, retrieving
  only the columns used in the expression; the row retrieved may be restricted
  by specifying an SQL query in the rows argument.
}
\usage{
\method{with}{odbcDataset}(data, expr, rows, ...)
}

\arguments{
  \item{data}{an odbcDataset object.}
  \item{expr}{an R expression to evaluate.}
  \item{rows}{an optional quoted SQL query to select rows from the dataset.}
  \item{\dots}{not used.}
}
\details{
  Perhaps specify some more information
}
\value{
  The result of evaluating \code{expr}.
}

\author{John Fox}

\seealso{\code{\link[RODBC]{odbcDriverConnect}}, \code{\link{Prestige}}.}

\examples{
## These examples presuppose that the data set prestige is stored as a table
## in the database attached via odbcDriverConnect().
## This data set is supplied as the data frame Prestige in the dfdb.RODBC
## package.
## Depending upon your OS/DBMS, the call to odbcDriverConnect() may have
## to be modified.

\dontrun{
channel <- odbcDriverConnect()
            
sqlTables(channel)

Prestige <- odbcDataset(channel, "prestige")  # create a pseudo-dataframe
with(Prestige, mean(income))
with(Prestige, mean(income), rows="type = 'prof'")
mod <- with(Prestige, lm(prestige ~ income + education))
summary(mod)
  }
}

\keyword{manip}
