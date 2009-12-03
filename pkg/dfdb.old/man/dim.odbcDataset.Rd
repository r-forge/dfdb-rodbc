\name{dim.odbcDataset}
\alias{dim.odbcDataset}

\title{Size of odbcDataset Object}
\description{
  Returns the number of rows and columns in the database table corresponding
  to the odbcDataset object
}

\usage{
\method{dim}{odbcDataset}(x)
}

\arguments{
  \item{x}{an odbcDataset object.}
}

\value{
The number of rows and columns in the odbcDataset object.
}

\author{John Fox}

\seealso{\code{\link{odbcDataset}}, \code{\link[RODBC]{odbcDriverConnect}}, \code{\link{Prestige}}.}

\examples{
## This example presupposes that the data set prestige is stored as a table
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
dim(Prestige)
  }
}

\keyword{manip}

