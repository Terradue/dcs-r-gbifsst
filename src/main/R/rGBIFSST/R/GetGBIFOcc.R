#' retrieves the GBIF occurrences using rgbif 
#' @description retrieves the GBIF occurrences of a given species belonging to a given kingdom using rgbif 
#'
#' @param name species name. Defaults to "Carcharodon carcharias"
#' @param kingdom species kingdom. Defaults to "animalia"
#' @param limit number of occurrences to retrieve. Defaults to 1000
#' @return occurrences data frame 
#' @examples \dontrun{
#' GetGBIFOcc("Carcharodon carcharias", "animalia", limit=100) 
#' }
#'
#' @export
#' @import rgbif

GetGBIFOcc <- function (name='Carcharodon carcharias', kingdom='animalia', limit=1000) {

  # get the occurrences from GBIF using rgbif
  key <- name_backbone(name=name, kingdom=kingdom)$speciesKey
  occ <- occ_search(taxonKey=key, limit=limit, return='data')
  occ <- occ[complete.cases(occ),]
  
  # bind the lon/lat columns
  occ <- cbind(occ$decimalLongitude, occ$decimalLatitude)

  return(occ)

}
