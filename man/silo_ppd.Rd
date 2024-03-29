% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SILO.R
\name{silo_ppd}
\alias{silo_ppd}
\title{Retrieve patched point data from LongPaddock}
\usage{
silo_ppd(
  station,
  start,
  finish = format(Sys.time() + 86400, "\%Y\%m\%d"),
  format = "standard",
  username
)
}
\arguments{
\item{station}{The station ID.}

\item{start}{The start date in the format "YYYYMMDD".}

\item{finish}{The finish date in the format "YYYYMMDD".}

\item{format}{The format of the data. The default is "standard".}

\item{username}{The username for LongPaddock.}
}
\value{
A tibble with the patched point data.
}
\description{
This function retrieves patched point data from LongPaddock. The function takes
the station ID, start date, finish date, format, and username as input. The
function returns a tibble with the patched point data.
}
\examples{
df <- silo_ppd(station = "32043",
                     start = format(Sys.time() - (365*86400), "\%Y\%m\%d"),
                     format = "standard",
                     username = "example@email.com.au")

}
\seealso{
https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php
}
