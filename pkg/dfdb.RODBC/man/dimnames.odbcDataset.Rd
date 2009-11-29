\name{dimnames.odbcDataset}
\alias{dimnames.odbcDataset}
\alias{names.odbcDataset}
\alias{row.names.odbcDataset}

\title{Row and Column Names of an odbcDataset Object}

\description{
\code{dimnames} returns the row and column names of an odbcDataset object;
\code{names} returns the column names;
\code{rownames} returns the row names.
}

\usage{
\method{dimnames}{odbcDataset}(x)
\method{names}{odbcDataset}(x)
\method{row.names}{odbcDataset}(x)
}

\arguments{
  \item{x}{an odbcDataset object.}
}

\value{
In the case of \code{names}, a character vector of column names;
in the case of \code{row.names}, a character vector of row names;
in the case of \code{dimnames}, a two-element list containing character
vectors of row names and column names. 
}

\author{John Fox}

\seealso{\code{\link{odbcDataset}}, \code{\link[RODBC]{odbcDriverConnect}}}

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
Prestige
names(Prestige)
row.names(Prestige)
dimnames(Prestige)
  }
}

\keyword{manip}