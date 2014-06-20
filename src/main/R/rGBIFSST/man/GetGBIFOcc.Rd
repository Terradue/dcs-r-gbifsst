\name{GetGBIFOcc}
\alias{GetGBIFOcc}
\title{retrieves the GBIF occurrences using rgbif}
\usage{
GetGBIFOcc(name = "Carcharodon carcharias", kingdom = "animalia",
  limit = 1000)
}
\arguments{
  \item{name}{species name. Defaults to "Carcharodon
  carcharias"}

  \item{kingdom}{species kingdom. Defaults to "animalia"}

  \item{limit}{number of occurrences to retrieve. Defaults
  to 1000}
}
\value{
occurrences data frame
}
\description{
retrieves the GBIF occurrences of a given species belonging
to a given kingdom using rgbif
}
\examples{
\dontrun{
GetGBIFOcc("Carcharodon carcharias", "animalia", limit=100)
}
}

