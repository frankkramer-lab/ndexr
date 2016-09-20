##Authors:
#   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
#   Dexter Pratt [depratt@ucsd.edu]
#   Frank Kramer [frank.kramer@med.uni-goettingen.de]
##Created: 6 June 2014
# Contains functions to search and retrieve networks

##########################################################
# Network Functions

#' Search networks in NDEx (by description)
#'     network    POST    /network/search/{skipBlocks}/{blockSize}    SimpleNetworkQuery    NetworkSummary[]
#' 
#' @param ndexcon object of class NDEXConnection
#' @param searchString string by which to search
#' @param accountName string; constrain search to networks administered by this account
#' @param skipBlocks -
#' @param blockSize -
#' @return Data frame with network information: ID, name, whether it is public, edge and node count; source and format of network. NULL if no networks are found.
#' @note Search strings may be structured
#' @examples \dontrun{ndex.find.networks("p53")}
#' @export
ndex.find.networks <- function(ndexcon, searchString="", accountName, skipBlocks = 0, blockSize = 10){

  ##Form JSON to post
  if (missing(accountName)){
    query <- jsonlite::toJSON(list(searchString=searchString), pretty=T, auto_unbox = T)
  } else {
    query <- jsonlite::toJSON(list(searchString=searchString, accountName=accountName), pretty=T, auto_unbox = T)
  }
  
  ##Form route
  route <- sprintf("/network/search/%s/%s", skipBlocks, blockSize)
  
  ##Get a list of NetworkSummary objects
  response <- ndex_rest_POST(ndexcon, route=route, query)
  
  if(is.data.frame(response)){
    return(response)
  } else {
    return(NULL)
  }
}


#' Get NetworkSummary by Network UUID
#' GET    /network/{networkUUID}       NetworkSummary
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @return List of network metadata: ID, name, whether it is public, edge and node count; source and format of network
#' @export
ndex.get.network.summary <- function(ndexcon, network_id){
  route <- paste0("/network/", network_id)
  response <- ndex_rest_GET(ndexcon, route)
  return(response)
}


#' Get complete network
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @return \code{\link{RCX}} object
#' @details Uses getEdges (this procedure will return complete network with all elements)
#' Nodes use primary ID of the base term ('represents' element)
#' Edges use primary ID of the base term ('predicate', or 'p' element)
#' Mapping table for the nodes is retrieved ('alias' and 'related' terms) to facilitate conversions/data mapping
#' @section REST query:
#' This function runs GET query /network/{networkUUID}/asCX (returns CX Network object)
#' @export
ndex.get.complete.network <- function(ndexcon, network_id){
  route <- paste0("/network/", network_id, "/asCX")
  response = ndex_rest_GET(ndexcon, route, raw=T)
  #CXnetwork = RCX.json2CX(response)
  #return(CXnetwork)
  return(response)
}

