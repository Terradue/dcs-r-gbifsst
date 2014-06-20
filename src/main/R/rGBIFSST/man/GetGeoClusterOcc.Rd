\name{GetGeoClusterOcc}
\alias{GetGeoClusterOcc}
\title{returns the geo-spatial envelope of the clusters calculated with the DBSCAN algorithm}
\usage{
GetGeoClusterOcc(occurrences, eps = 5, minpts = 20)
}
\arguments{
  \item{occurrences}{Data frame as returned by
  GetGBIFOcc()}

  \item{eps}{Reachability distance, see Ester et al.
  (1996)}

  \item{minpts}{Reachability minimum no. of points, see
  Ester et al. (1996).}
}
\value{
list of geospatial envelope in WKT format
}
\description{
returns the geo-spatial envelope of the clusters calculated
with the DBSCAN algorithm
}
\examples{
\dontrun{
GetGeoClusterOcc(GetGBIFOcc("Carcharodon carcharias", "animalia", limit=100), eps=3, minpts=10)
}
}

