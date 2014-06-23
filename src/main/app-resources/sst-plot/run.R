#!/usr/bin/Rscript --vanilla --slave --quiet

# load rciop library to access the developer cloud sandbox functions
library("rciop")
library("stringr")
library("raster")
library("httr")
library("RJSONIO")

# load the application package when mvn installed it
library(rGBIFSST, lib.loc="/application/share/R/library")

# load any other R library required
library("rgeos")

# load the parametee values with rciop.getparam() function
netcdf.variable <- rciop.getparam("netcdf.variable")
thredds <- rciop.getparam("thredds")

# add a log message
rciop.log("DEBUG", paste("I'm running a job with parameter values:", netcdf.variable, thredds)) 

wcs.template <- GetWCSTemplate()

wcs.template$value[wcs.template$param == "service"] <- "WCS"
wcs.template$value[wcs.template$param == "version"] <- "1.0.0"
wcs.template$value[wcs.template$param == "request"] <- "GetCoverage"
wcs.template$value[wcs.template$param == "format"] <- "NetCDF3"
wcs.template$value[wcs.template$param == "coverage"] <- netcdf.variable

# get URLs from thredds
thredds.urls <- GetThreddsURL(thredds)

# read the inputs coming from stdin
f <- file("stdin")
open(f)

while(length(bbox <- readLines(f, n=1)) > 0) {
  
  rciop.log("INFO", paste("processing bbox", bbox))
  
  bbox <- str_replace_all(bbox, "[![:cntrl:]]", "")

  wcs.template$value[wcs.template$param == "bbox"] <- bbox
  
  r.stack <- c()
  idx <- c()
  
  for (i in 1:5) { #length(thredds.urls)) {
  
    attempt <- 1
    r <- NULL
  
    rciop.log("INFO", paste("url:", thredds.urls[[i]]))
    
    # attempt a few times, the WCS server can be busy
    while ( is.null(r) && attempt <= 10 ) {
      attempt <- attempt + 1
      possible.error <- tryCatch(
        r <- GetWCSCoverage(thredds.urls[[i]], wcs.template, by.ref=FALSE),
          error=function(e) e
        )
  
      if(inherits(possible.error, "error")) {
        rciop.log("WARN","sleeping")
        Sys.sleep(time=5)
      }
    }
    
    if (is.null(r)) next
    d <- r.stack <- c(r.stack, r)  
    
    # update the index breaking down the thredds path and extrating/formatting the date
    # the date is part of the URL path and provided as Y-j
    path_split <- unlist(str_split(string=parse_url(thredds.urls[[i]])$path, pattern="/"))
    rciop.log("INFO", strptime(paste(path_split[8], path_split[9], sep="-"), "%Y-%j"))
    idx <- c(idx, as.character(strptime(paste(path_split[8], path_split[9], sep="-"), "%Y-%j")))
  }
  
  my.stack <- setZ(stack(r.stack), idx)
  names(my.stack) <- idx
  
  stack.mean <- cellStats(my.stack, 'mean')
  names(stack.mean) <- NULL

  # convert bbox to numeric vector and create a matrix with the coordinates
  bbox <- as.numeric(unlist(str_split(bbox, pattern=",")))
  coord <- list(matrix(c(bbox[1], bbox[2], bbox[1], bbox[4], bbox[3], bbox[4], bbox[3], bbox[2], bbox[1], bbox[2]), nrow=5, ncol=2, byrow=TRUE))
 
  stack.list <- list(type="FeatureCollection", features=list(type="Feature", geometry=list(type="Polygon", coordinates=coord) , 
		properties=list(obsTypeDesc="Sea Surface Temperature", 
				obsType="analysed_sst", 
				uomType="kelvin",
				time=idx, 
				value=stack.mean)))

  
  json.file <- paste(tempfile(pattern = "file", tmpdir=TMPDIR), ".json", sep="")
  writeLines(toJSON(stack.list, pretty=TRUE), json.file)
  
  # publish the json file generated 
  rciop.publish(json.file, recursive=FALSE, metalink=TRUE)
 
}
