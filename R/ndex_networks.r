################################################################################
## Authors:
##   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##   Dexter Pratt [depratt@ucsd.edu]
##   Frank Kramer [frank.kramer@med.uni-goettingen.de]
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 6 June 2014 by Ishkin
##   Split on 25 January 2017 by Auer
## 	
## Description:
##	Contains functions to search and retrieve networks
################################################################################


##########################################################
###
###   Network Functions
###
##########################################################


#' Search networks in NDEx (by description)
#' 
#' This functions searches the public networks on an NDEx server for networks containing the supplied search string. T
#' his search can be limited to certain accounts as well as in length.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param searchString string by which to search
#' @param accountName string (optional); constrain search to networks administered by this account
#' @param start integer (optional); specifies that the result is the nth page of the requested data. The default value is 0
#' @param size integer (optional); specifies the number of data items in each page. The default value is 100
#' @return Data frame with network information: ID, name, whether it is public, edge and node count; source and format of network. NULL if no networks are found.
#' @section REST query:
#' This function runs POST query /network/search/{start}/{size}    returns list of NetworkSummary
#' @note Search strings may be structured
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws1 = ndex.find.networks(ndexcon1,"p53") }
#' @export
ndex.find.networks <- function(ndexcon, searchString="", accountName, start=0, size=100){

  ##Form JSON to post
  query = list(searchString=searchString)
  if (!missing(accountName)){
    query$accountName=accountName
  }
  query <- jsonlite::toJSON(query, pretty=T, auto_unbox = T)
  
  ##Form route
  ## ToDo: somehow the 1.3 api changed?! old version:
  ## route <- sprintf("/network/search/%s/%s", start, size)
  ## now somehow it changed to "http://public.ndexbio.org/rest/network/textsearch/0/1000" (from Chrome, 28.Nov.2016)
  api = ndex.conf$search$network$search 
  if(ndexcon$apiversion=='2.0'){
    route <- ndex.helper.UrlAddParams(api$'2.0'$url, c('start','size'), c(start,size))
  }else{
    route <- api$'1.3'$url
    route <- gsub(ndex.conf$replaceables$start,start, route)
    route <- gsub(ndex.conf$replaceables$size, size, route)
  }
  
  
  ##Get a list of NetworkSummary objects
  response <- ndex_rest_POST(ndexcon, route=route, data=query)
  response = response$networks

  if(length(response) > 0){
    return(response)
  } else {
    return(NULL)
  }
}