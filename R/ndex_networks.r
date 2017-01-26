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
#' @param accountName string; constrain search to networks administered by this account
#' @param skipBlocks -
#' @param blockSize -
#' @return Data frame with network information: ID, name, whether it is public, edge and node count; source and format of network. NULL if no networks are found.
#' @section REST query:
#' This function runs POST query /network/search/{skipBlocks}/{blockSize}    returns list of NetworkSummary
#' @note Search strings may be structured
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws1 = ndex.find.networks(ndexcon1,"p53") }
#' @export
ndex.find.networks <- function(ndexcon, searchString="", accountName, skipBlocks = 0, blockSize = 10){

  ##Form JSON to post
  query = list(searchString=searchString)
  if (!missing(accountName)){
    query$accountName=accountName
  }
  query <- jsonlite::toJSON(query, pretty=T, auto_unbox = T)
  
  ##Form route
  ## ToDo: somehow the 1.3 api changed?! old version:
  ## route <- sprintf("/network/search/%s/%s", skipBlocks, blockSize)
  ## now somehow it changed to "http://public.ndexbio.org/rest/network/textsearch/0/1000" (from Chrome, 28.Nov.2016)
  api = ndex.api$search$network$search 
  if(ndexcon$apiversion=='2.0'){
    route <- ndex.helper.UrlAddParams(api$'2.0'$url, c('start','size'), c(skipBlocks,blockSize))
  }else{
    route <- api$'1.3'$url
    route <- gsub(ndex.api$replaceables$start,skipBlocks, route)
    route <- gsub(ndex.api$replaceables$size, blockSize, route)
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