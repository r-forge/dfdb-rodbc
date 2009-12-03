\name{as.data.frame.odbcDataset}
\alias{as.data.frame.odbcDataset}
\alias{as.list.odbcDataset} 
\alias{as.matrix.odbcDataset}

\title{Coerce an odbcDataset Object to Another Form}

\description{
These functions coerce odbcDataset objects to data frames, lists, or matrices.
Because the whole data set is returned, these aren't of much use.
}

\usage{
\method{as.data.frame}{odbcDataset}(x, row.names, optional, \ldots)
\method{as.list}{odbcDataset}(x, \ldots)
\method{as.matrix}{odbcDataset}(x, \ldots)
}


\arguments{
  \item{x}{an odbdDataset object.}
  \item{row.names, optional, \ldots}{ignored.}
}

\value{
  A data frame, list, or matrix.
}

\author{John Fox}

\seealso{\code{\link{odbcDataset}}, \code{\link[RODBC]{odbcDriverConnect}}, \code{\link{Prestige}}.}

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
as.data.frame(Prestige)
as.list(Prestige)
as.matrix(Prestige) # note: all entries coerced to character
  }
}

\keyword{manip}

