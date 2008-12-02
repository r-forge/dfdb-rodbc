\name{Extract.odbcDataset}
\alias{$.odbcDataset}
\alias{[[.odbcDataset}
\alias{[.odbcDataset}

\title{Extracts Parts of an odbcDataset Object}

\description{
 These operators are used to extract subsets of rows and/or columns of an
 odbcDataset object.
}

\usage{
\method{$}{odbcDataset}(x, i)
\method{[[}{odbcDataset}(x, i)
\method{[}{odbcDataset}(x, i, j, drop = TRUE, \ldots)
}

\arguments{
  \item{x}{an odbcDataset object.}
  \item{i}{for \code{$} or \code{[[}, the name of a column in the dataset,
    quoted in the case of \code{[[}. For \code{[}, a quoted
    SQL query to select a subset of rows; if missing, all rows are selected.}
  \item{j}{columns to select; may be column numbers, negative numbers, or names
  	if missing, all columns are selected.}
  \item{drop}{is a single column is selected and \code{drop = TRUE}, then
    a vector, rather than a one-column data frame, is returned.}
  \item{\ldots}{not used.}
}

\details{
  Perhaps some more explanation here.
}

\value{
 These operators return a data frame or vector.
}


\author{John Fox}

\seealso{\code{\link{odbcDataset}}, \code{\link[RODBC]{odbcDriverConnect}}, \code{\link{Prestige}}.}

\examples{
## These example presuppose that the data set prestige is stored as a table
## in the database attached via odbcDriverConnect().
## This data set is supplied as the data frame Prestige in the dfdb.RODBC
## package.
## Depending upon your OS/DBMS, the call to odbcDriverConnect() may have
## to be modified.

\dontrun{
channel <- odbcDriverConnect()
            
sqlTables(channel)

Prestige <- odbcDataset(channel, "prestige")  # create a pseudo-dataframe
Prestige

Prestige[,]   # retrieve the whole dataset as a data frame

Prestige$income

Prestige[["income"]]

Prestige["type = 'prof'",]    # perform an SQL fetch

Prestige["type = 'prof' order by income", c("education", "income")]

mod <- lm(prestige ~ income + education, 
    data=Prestige[,c("income", "education", "prestige")])
summary(mod)

remove(Duncan, mod, channel)

  }
}

\keyword{manip}
