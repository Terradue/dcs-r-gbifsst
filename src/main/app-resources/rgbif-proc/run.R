#!/usr/bin/Rscript --vanilla --slave --quiet

library(rciop)
library(rgbif)
library(stringr)
library(fpc)
library(rGBIFSST)

# get the species info
species <- rciop.getparam("species")
eps <- rciop.getparam("eps")
minpts <- rciop.getparam("minpts")

# breack the species info into species and kingdom
name <- str_split(species, pattern=":")[[1]][1]
kingdom <- str_split(species, pattern=":")[[1]][2]

# get the occurrences from GBIF with rgbif
occ <- rGBIFSST::GetGBIFOcc(name=name, kingdom=kingdom)

# get the minimum bounding boxes for each cluster detected by the DBSCAN algorithm 
mbr <- rGBIFSST::GetGeoClusterOcc(occ, eps=eps, minpts=minpts)

# mbr contains spatialPolygons, get them as xmin, ymin, xmax, ymax
bbox <- unlist(lapply(X=mbr, function(x) {
  xmin <- x@bbox[1,1]
  xmax <- x@bbox[1,2]
  ymin <- x@bbox[2,1]
  ymax <- x@bbox[2,2]
  return(paste(xmin, ymin, xmax, ymax, sep=","))
}
))

# publish the minimum bounding boxes as inputs 
# for the next processing step
rciop.publish(path=cat(bbox, sep="\n"), mode="silent")
