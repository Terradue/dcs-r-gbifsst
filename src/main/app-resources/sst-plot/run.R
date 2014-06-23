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

# unit test
#url=c("http://data.nodc.noaa.gov/thredds/wcs/ghrsst/L4/GLOB/UKMO/OSTIA/2013/364/20131230-UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc.bz2", 
#  "http://data.nodc.noaa.gov/thredds/wcs/ghrsst/L4/GLOB/UKMO/OSTIA/2013/365/20131231-UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc.bz2")
#  date=c("20131230", "20131231") 

#thredds.urls <- data.frame(url, date, stringsAsFactors=FALSE)

# read the inputs coming from stdin
f <- file("stdin")
open(f)

while(length(bbox <- readLines(f, n=1)) > 0) {
  
  rciop.log("INFO", paste("processing bbox", bbox))
  
  rciop.log("DEBUG", paste("escaped", str_replace_all(bbox, "[![:cntrl:]]", ""), "aaa", sep="")) 

  wcs.template$value[wcs.template$param == "bbox"] <- str_replace_all(bbox, "[![:cntrl:]]", "")
  
  r.stack <- c()
  idx <- c()
  
  for (i in 1:10) { #length(thredds.urls)) {
  
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
 
  stack.list <- list(type="FeatureCollection", features=list(type="Feature", geometry=list(type="Polygon", extent=str_replace_all(bbox, "[![:cntrl:]]", "")) , 
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
