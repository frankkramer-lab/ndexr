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
  rcx = ndex.JSON2RCX(response)
  return(rcx)
}


#' Get Network Provenance by Network UUID
#' GET    /network/{networkUUID}/provenance       Provenance
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @return List of network metadata: ID, name, whether it is public, edge and node count; source and format of network
#' @export
ndex.get.network.provenance <- function(ndexcon, network_id){
  route <- paste0("/network/", network_id,"/provenance")
  response <- ndex_rest_GET(ndexcon, route)
  return(response)
}

#### TODO IMPLEMENT THESE ( from http://www.home.ndexbio.org/using-the-ndex-server-api/ )

## Update the Network Profile Information
## POST : /network/{networkId}/summary
## 
## Update the Properties of a Network
## PUT : /network/{networkId}/properties
## 
## Modify the Provenance History for a Network
## PUT : /network/{networkId}/provenance


#' Set a Network Read-Only
#' GET : /network/{networkId}/setFlag/{parameter}={value}
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @param readonly logical, defaults to TRUE
#' @return response from server
#' @export
ndex.set.network.readonly <- function(ndexcon, network_id, readonly=c(TRUE,FALSE)[1]){
  if(readonly) {
    route <- paste0("/network/", network_id,"/setFlag/readOnly=true")
  } else {
    route <- paste0("/network/", network_id,"/setFlag/readOnly=false")
  }
  response <- ndex_rest_GET(ndexcon, route)
  return(response)
}

## Delete a Network
## DELETE :  /network/{networkId}
## 
## Get Selected Aspects of a Network as CX
## POST : /network/{networkId}/aspects


#' Get Some Aspect Elements from a Network as CX
#' 
#' GET : /network/{networkId}/aspect/{aspectName}/{limit}
#' This method returns elements in the specified aspect up to the specified limit.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @param aspectName string name of the aspect to retrieve
#' @param limit integer limiting the amount of output to retrieve
#' @return \code{\link{RCX}} object
#' @section REST query:
#' GET : /network/{networkId}/aspect/{aspectName}/{limit}
#' @export
ndex.get.limited.aspect <- function(ndexcon, network_id, aspectName, limit){
  route <- paste0("/network/", network_id, "/aspect/",aspectName,"/",limit)
  response = ndex_rest_GET(ndexcon, route, raw=T)
  rcx = ndex.JSON2RCX(response)
  return(rcx)
}

#### TODO IMPLEMENT THIS
## Query a Network neighborhood as CX
## POST : /network/{networkId}/asCX/query
## 

#' Create a Network from CX data
#' 
#' POST : /network/asCX
#' TODO: currently doesnt seem to be working.Debug!
#' This method creates a new network on the NDEx server.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param rcx \code{\link{RCX}} object
#' @return \code{\link{RCX}} object
#' @section REST query:
#' POST : /network/asCX
#' @export
ndex.create.network <- function(ndexcon, rcx){
  route <- paste0("/network/asCX")
  postdata = toJSON(list(
                          CXNetworkStream=
                           list("filename",
                                rcx_back1,
                                "application/octet-stream")
                         )
                    )
  response = ndex_rest_POST(ndexcon, route, raw=T)
  return(response)
}


## 
## Update an Entire Network as CX
### PUT : /network/asCX
## 
## Archive a BEL Namespace File in a Network
## POST : /network/{networkId}/namespace

## Get an Archived BEL Namespace File from a Network
## GET : /network/{networkId}/namespaceFile/{prefix}
## 
## DOES THIS WORK? What to use for prefix? Prefix as listet in ndex.get.network.namespace doesnt work?
## # 
## > route <- paste0("/network/", network_id,"/namespaceFile/HGNC")
## > response <- ndexr:::ndex_rest_GET(ndexcon, route)
### 
## GET: [ http://www.ndexbio.org/rest/network/eac8a4b8-6194-11e5-8ac5-06603eb7f303/namespaceFile/HGNC ]
## 
## Response:{"errorCode":"NDEx_Object_Not_Found_Exception","message":"Namespace file of HGNC not found in this network.","description":null,"stackTrace":"java.lang.Thread.getStackTrace(Thread.java:1552)\norg.ndexbio.model.errorcodes.NDExError.stackTrace(NDExError.java:103)\norg.ndexbio.model.errorcodes.NDExErro...


#' Get the BEL Namespaces Associated with a Network UUID
#' GET : /network/{networkId}/namespace
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @return data.frame listing network namespace
#' @export
ndex.get.network.namespace <- function(ndexcon, network_id){
  route <- paste0("/network/", network_id,"/namespace")
  response <- ndex_rest_GET(ndexcon, route)
  return(response)
}


#' Get the Metadata Associated with a Network UUID
#' GET : /network/{networkId}/metadata
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @return data.frame listing network metadata: ID, name, whether it is public, edge and node count; source and format of network
#' @export
ndex.get.network.metadata <- function(ndexcon, network_id){
  route <- paste0("/network/", network_id,"/metadata")
  response <- ndex_rest_GET(ndexcon, route)
  return(response)
}