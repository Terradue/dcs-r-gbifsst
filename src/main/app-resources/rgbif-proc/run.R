#!/usr/bin/Rscript --vanilla --slave --quiet

library(rciop)
library(rgbif)
library(stringr)
library(fpc)
library(sp)
library(rgdal)

# load the application package where mvn installed it
library(rGBIFSST, lib.loc="/application/share/R/library")

# get the species info
species <- rciop.getparam("species")
eps <- rciop.getparam("eps")
minpts <- rciop.getparam("minpts")

rciop.log("INFO", paste("Get geo-spatial clusters for species", species, "using eps:", eps, "minpts:", minpts))

# get the occurrences from GBIF with rgbif
occ <- as.data.frame(GetGBIFOcc(name=species))

# get the minimum bounding boxes for each cluster detected by the DBSCAN algorithm 
mbr <- GetGeoClusterOcc(occ, eps=eps, minpts=minpts)

# mbr contains spatialPolygons, get them as xmin, ymin, xmax, ymax
bbox <- unlist(lapply(X=mbr, function(x) {
  xmin <- x@bbox[1,1]
  xmax <- x@bbox[1,2]
  ymin <- x@bbox[2,1]
  ymax <- x@bbox[2,2]
  return(paste(xmin, ymin, xmax, ymax, sep=","))
}
))

# create a geoJSON 
pol <- SpatialPolygonsDataFrame(mbr[[1]], data.frame(species=species))
for (n in 2:length(mbr)) {
 temp.pol <- SpatialPolygonsDataFrame(mbr[[n]], data.frame(species=species))
 pol <- rbind(pol, spChFIDs(temp.pol,as.character(n)))  
}
proj4string(pol) <- CRS("+init=epsg:4326")

# write the geoJSON
setwd(TMPDIR)
writeOGR(pol, "cluster.geojson", "pol", driver='GeoJSON')

# publish the geoJSON
rciop.publish(paste(TMPDIR,  "cluster.geojson", sep="/"), metalink=TRUE)

rciop.log("INFO", paste("Identified", length(bbox), "geo-spatial clusters"))

# publish the minimum bounding boxes as inputs 
# for the next processing step
rciop.publish(path=print(bbox), mode="silent")
