#' retrieves the 
#' @description retrieves the
#'
#' @param Url the url eligibles to collect the dataset
#' @examples \dontrun{
#' GetGBIFOcc("http://data.nodc.noaa.gov/thredds/catalog/ghrsst/L4/GLOB/UKMO/OSTIA/catalog.xml") 
#' }
#'
#' @export
#' @import httr
#' @import XML


GetThreddsURL<-function(Url)
{
     ##library(httr)
     ##library(XML)

     ## basic objects
     tCatalogue<-"thredds/catalog"
     tWcs<-"thredds/wcs"
     listOfUrls<-list()  ## the output list of url
     index<-1            ## the list index
     
     ## nested recoursive function "getUrl"
     getUrl<-function(url)
     {
          print(paste("getUrl read url:",url))
          baseUrl<-httr::parse_url(url)     ## get the related url's informations
          basePageUrl<-paste(baseUrl$scheme,"://",baseUrl$hostname, sep="")
          
          ## start reading the xml file
          xmlfile<-xmlTreeParse(url)            ## load the xml file by url
          rootNode = xmlRoot(xmlfile)
          
          ## check the basic codition. If the //dataset[@urlPath] is found, 
          ## then store every urlPath in the listOfUrls list (basic condition)
          productsUrl<-xmlSApply(rootNode, function(x) xmlSApply(x, xmlGetAttr, "urlPath"))
          productsUrlRef<-unlist(productsUrl$dataset)
          print(productsUrlRef)

          if(length(productsUrlRef) != 0)
          {
               ## basic condition
               productUrl<-paste(basePageUrl, tWcs, productsUrlRef, sep="/")
               print(productUrl)
               listOfUrls[index]<<-productUrl
               index<<-index+1
          }
          else
          {
               print("Complex condition")
               ## we are not in the basic condition, we need to explore the file to retrieve the link 
               ## to the basic condition retrieving all the ID attributes. Collecting the catalogRef[@ID]
               ids<-xmlSApply(rootNode, function(x) xmlSApply(x, xmlGetAttr, "ID"))
               catalogRef<-unlist(ids$dataset)
               innerCatalogUrl<-paste(basePageUrl,tCatalogue,catalogRef,"catalog.xml",sep="/")
               
               for(i in 1:length(innerCatalogUrl))
                    getUrl(innerCatalogUrl[i])
          }     
     }
     
     ## return the list of urls
     getUrl(Url)
     return(listOfUrls)
}