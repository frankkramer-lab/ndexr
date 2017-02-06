################################################################################
## Authors:
##   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##   Dexter Pratt [depratt@ucsd.edu]
##   Frank Kramer [frank.kramer@med.uni-goettingen.de]
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Split from ndex_networks on 25 January 2017 by Auer
## 	
## Description:
##	Contains functions to search and retrieve networks
################################################################################


####################################################
## 
##   Simple network operations
##
####################################################

#' Get NetworkSummary by Network UUID
#' 
#' This function retrieves the summary of the network identified by the supplied network UUID string.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @return List of network metadata: ID, name, whether it is public, edge and node count; source and format of network
#' @section REST query:
#' This function runs GET query /network/{networkUUID}    and returns a single NetworkSummary
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' ndex.get.network.summary(ndexcon,pws[1,"externalId"]) }
#' @export
ndex.get.network.summary <- function(ndexcon, network_id){
  route <- paste0("/network/", network_id)  #!ToDo: change to ndex.conf and test! No hard-coded urls!!
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
#' Compatible to NDEx server version 1.3 and 2.0
#' @section REST query:
#' This function runs GET query /network/{networkUUID}/asCX  and returns CX network object which is parsed into an RCX object
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.network(ndexcon,pws[1,"externalId"]) }
#' @export
ndex.get.network <- function(ndexcon, network_id){
  
  ## route <- paste0("/network/", network_id, "/asCX")
  api = ndex.conf$network$get 
  if(ndexcon$apiversion=='2.0'){
    route <- api$'2.0'$url
    route <- gsub(ndex.conf$replaceables$network,network_id, route)
  }else{
    route <- api$'1.3'$url
    route <- gsub(ndex.conf$replaceables$network,network_id, route)
  }
  
  response = ndex_rest_GET(ndexcon, route, raw=T)
  rcx = rcx.fromJSON(response)
  return(rcx)
}


#' Create a Network from CX data
#' 
#' This method creates a new network on the NDEx server.
#' Compatible to NDEx server version 1.3 and 2.0
#' 
#' @param ndexcon object of class NDEXConnection
#' @param rcx \code{\link{RCX}} object
#' @return \code{\link{RCX}} object
#' @section REST query:
#' POST : /network/asCX
#' @export
ndex.create.network <- function(ndexcon, rcx){
	route <- paste0("/network/asCX")
	api = ndex.conf$network$create
	if(ndexcon$apiversion=='2.0'){
		route <- api$'2.0'$url
	}else{
		route <- api$'1.3'$url
	}
	data <- c(CXNetworkStream = rcx.toJSON(rcx))
	response = ndex_rest_POST(ndexcon, route, data, multipart=T, raw=T)
	return(response)
}


#' Update an Entire Network as CX
#' 
#' PUT : /network/asCX
#' This method creates a new network on the NDEx server.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param rcx \code{\link{RCX}} object
#' @param network_id UUID of the network
#' @return \code{\link{RCX}} object
#' @section REST query:
#' POST : /network/asCX
#' @export
ndex.update.network <- function(ndexcon, rcx, network_id = NULL){	#!!ToDo: update to api 2.0
	if(is.null(network_id)){
		if(is.null(rcx$ndexStatus)|| is.null(rcx$ndexStatus$externalId)) {
			warning("ndex.update.network: no network id specified, whether as parameter nor in rcx$ndexStatus$externalId")
			return(NULL)
		}
		network_id = rcx$ndexStatus$externalId
	}
	route <- paste0("/network/asCX/",network_id)
	data <- c(CXNetworkStream = rcx.toJSON(rcx))
	response = ndex_rest_PUT(ndexcon, route, data, raw=T)
	return(response)
}
# *   Trying 52.26.53.110...
# * Connected to www.ndexbio.org (52.26.53.110) port 80 (#0)
#   * Server auth using Basic with user 'testacc'
#   > PUT /rest/network/asCX/1c6de696-9cef-11e6-9ed0-06603eb7f303 HTTP/1.1
#   Host: www.ndexbio.org
#   Authorization: Basic dGVzdGFjYzp0ZXN0YWNj
#   Accept: */*
#     Content-Type: application/json
#   Content-Length: 147975
#   Expect: 100-continue
#   
#   < HTTP/1.1 100 Continue
#   * We are completely uploaded and fine
#   < HTTP/1.1 502 Bad Gateway
#   < Date: Fri, 28 Oct 2016 10:41:07 GMT
#   < Server: Apache/2.4.23 (Amazon)
#   < Content-Length: 232
#   < Content-Type: text/html; charset=iso-8859-1
#   < 
#     * Connection #0 to host www.ndexbio.org left intact
#   Response:<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
#     <html><head>
#     <title>502 Bad Gateway</title>
#     </head><body>
#     <h1>Bad Gateway</h1>
#     <p>The proxy server received an invalid
#   response from an upstream server.<br />
#     </p>
#     </body></html>


#' Delete a network
#' @param ndexcon object of class NDEXConnection
#' @param network_id UUID of the network
#' @section REST query:
#' @export
ndex.delete.network <- function(ndexcon, network_id){	#!!: Implement! Code is just a copy of update.network
	if(is.null(network_id)){
		if(is.null(rcx$ndexStatus)|| is.null(rcx$ndexStatus$externalId)) {
			warning("ndex.update.network: no network id specified, whether as parameter nor in rcx$ndexStatus$externalId")
			return(NULL)
		}
		network_id = rcx$ndexStatus$externalId
	}
	route <- paste0("/network/asCX/",network_id)
	data <- c(CXNetworkStream = rcx.toJSON(rcx))
	response = ndex_rest_PUT(ndexcon, route, data, raw=T)
	return(response)
}