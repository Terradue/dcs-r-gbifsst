#' returns the geo-spatial envelope of the clusters calculated with the DBSCAN algorithm
#' @description  returns the geo-spatial envelope of the clusters calculated with the DBSCAN algorithm
#'
#' @param occurrences Data frame as returned by GetGBIFOcc()
#' @param eps Reachability distance, see Ester et al. (1996)
#' @param minpts Reachability minimum no. of points, see Ester et al. (1996).
#' @return list of geospatial envelope in WKT format
#' @examples \dontrun{
#' GetGeoClusterOcc(GetGBIFOcc("Carcharodon carcharias", "animalia", limit=100), eps=3, minpts=10)
#' }
#'
#' @export
#' @import fpc
#' @import rgeos

GetGeoClusterOcc <- function(occurrences, eps=5, minpts=20) {

  mbr <- list()
  
  ### dbscan clustering 
  dbscan.res <- dbscan(occurrences, eps=eps, MinPts=minpts)

  #extract the minimum bounding box for each cluster 
  for(i in 1:max(dbscan.res$cluster)) {

    # get the cluster 
    cl <- (occurrences[ds$cluster %in% i,])
  
    # create the matrix with the cluster minimum bounding box
    coords <- matrix(nrow=5, ncol=2, byrow=TRUE, data=c(
      min(cl[,1]), min(cl[,2]), 
      max(cl[,1]), min(cl[,2]), 
      max(cl[,1]), max(cl[,2]), 
      min(cl[,1]), max(cl[,2]), 
      min(cl[,1]), min(cl[,2])))
  
    # get the cluster geospatial envelope
    mbr[[i]] <- gEnvelope(SpatialPoints(coords))
    
  }
  
  return(mbr)
  
}