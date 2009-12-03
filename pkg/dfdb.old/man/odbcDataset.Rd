\name{odbcDataset}
\alias{odbcDataset}
\alias{print.odbcDataset}
\alias{summary.odbcDataset}

\title{Create an odbcDataset Object}
\description{
This functions creates an odbcDataset object linked to a table in an 
ODBC-accessible database.  The \code{print} method normally prints
information about the data set; the \code{summary} method summarizes it
as a data frame.
}

\usage{
odbcDataset(channel, table, rownames)
\method{print}{odbcDataset}(x, ..., verbose = FALSE)
\method{summary}{odbcDataset}(object, ...)
}

\arguments{
  \item{channel}{a channel to the database, as returned by 
    \code{\link[RODBC]{odbcConnect}}.}
  \item{table}{the quoted name of a database table.}
  \item{rownames}{\code{TRUE} if the first character variable 
    in the data base contains row names; \code{FALSE} if there is no row names 
    variable; if not specified, the function will look for a suitable variable 
    in the data base}
  \item{x, object}{an object of class \code{odbcDataset}.}
  \item{verbose}{if \code{FALSE}, the default, print information about the
    odbcDataset object; if \code{TRUE}, print the whole object as a data
    frame.}
  \item{\dots}{ignored.}
}
\details{
  Possibly further description here.
}
\value{
  An object of class \code{odbcDataset}, with the following components:
  \item{channel}{The ODBC channel to the database on which the table resides.}
  \item{table}{The name of the database table.}
  \item{rownames}{\code{TRUE} or \code{FALSE}.}
  \item{column.selector}{The position of the variable names.}
  }

\author{John Fox}

\seealso{\code{\link[RODBC]{odbcDriverConnect}}, \code{\link{Prestige}}.}

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
unclass(Prestige)
print(Prestige, verbose=TRUE)
summary(Prestige)
  }
}

\keyword{manip}

