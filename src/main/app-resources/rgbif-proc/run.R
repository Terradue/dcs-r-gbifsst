#!/usr/bin/Rscript --vanilla --slave --quiet

library(rciop)
library(rgbif)
library(stringr)
library(fpc)
#library(sp)

# load the application package when mvn installed it
library(rGBIFSST, lib.loc="/application/share/R/library")

# get the species info
species <- rciop.getparam("species")
eps <- rciop.getparam("eps")
minpts <- rciop.getparam("minpts")

# breack the species info into species and kingdom
name <- str_split(species, pattern=":")[[1]][1]
kingdom <- str_split(species, pattern=":")[[1]][2]

rciop.log("INFO", paste("Get geo-spatial clusters for species", name, "of kingdom", kingdom, "using eps:", eps, "minpts:", minpts))

# get the occurrences from GBIF with rgbif
occ <- as.data.frame(GetGBIFOcc(name=name, kingdom=kingdom))

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

str(bbox)

rciop.log("INFO", paste("Identified", length(bbox), "geo-spatial clusters"))

# publish the minimum bounding boxes as inputs 
# for the next processing step
rciop.publish(path=print(bbox), mode="silent")
