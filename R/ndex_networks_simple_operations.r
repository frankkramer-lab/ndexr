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
#' GET: ndex.api.config$api$network$summary$get
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' ndex.network.get.summary(ndexcon,pws[1,"externalId"]) }
#' @export
ndex.network.get.summary <- function(ndexcon, network_id){
  api = ndex.helper.getApi(ndexcon, 'network$summary$get')
  route <- ndex.helper.encodeParams(api$url, api$params, network_id)
  
  response = ndex_rest_GET(ndexcon, route)
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
#' GET: ndex.api.config$api$network$get
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.network(ndexcon,pws[1,"externalId"]) }
#' @export
ndex.get.network <- function(ndexcon, network_id){
  api = ndex.helper.getApi(ndexcon, 'network$get')
  route <- ndex.helper.encodeParams(api$url, api$params, network_id)
  
  response = ndex_rest_GET(ndexcon, route, raw=T)
  rcx = rcx.fromJSON(response)
  return(rcx)
}


#' Create a Network at a server from RCX data
#' 
#' @param ndexcon object of class NDEXConnection
#' @param rcx \code{\link{RCX}} object
#' @return UUID of the newly created network
#' @details This method creates a new network on the NDEx server from the given RCX object
#' @section REST query:
#' POST (multipart/form-data): ndex.api.config$api$network$create$url
#' data: CXNetworkStream = data
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @export
ndex.create.network <- function(ndexcon, rcx){
  api = ndex.helper.getApi(ndexcon, 'network$create')
  route <- api$url
  
  tmpFile = tempfile()
  writeLines(rcx.toJSON(rcx, pretty = T), tmpFile)
  data <- list(CXNetworkStream = upload_file(tmpFile, type = 'application/json'))
	response = ndex_rest_POST(ndexcon, route, data, multipart=T, raw=T)
	## response is the url of the network: "http://public.ndexbio.org/v2/network/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
	## only the uuid should be returned
	uuid = tail(unlist(strsplit(response, '/')), n=1)
	file.remove(tmpFile)
	return(uuid)
}


#' Update an Entire Network as CX
#' 
#' This method updates/replaces a existing network on the NDEx server with new content.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param rcx \code{\link{RCX}} object
#' @param nuuid UUID of the network
#' @return UUID of the updated network
#' @details This method updates a network on the NDEx server with new content from the given RCX object
#' @section REST query:
#' PUT (multipart/form-data): ndex.api.config$api$network$update$url
#' data: CXNetworkStream = data
#' @export
ndex.update.network <- function(ndexcon, rcx, nuuid){
	if(missing(nuuid)){
		if(is.null(rcx$ndexStatus)|| is.null(rcx$ndexStatus$externalId)) {
			stop("ndex.update.network: no network id specified, whether as parameter nor in rcx$ndexStatus$externalId")
		}
	  nuuid = rcx$ndexStatus$externalId
	}
		
	api = ndex.helper.getApi(ndexcon, 'network$update')
	route <- ndex.helper.encodeParams(api$url, api$params, nuuid)
	
	tmpFile = tempfile()
	writeLines(rcx.toJSON(rcx, pretty = T), tmpFile)
	data <- list(CXNetworkStream = upload_file(tmpFile, type = 'application/json'))
	response = ndex_rest_PUT(ndexcon, route, data, multipart=T, raw=T)
	file.remove(tmpFile)
	return(nuuid)
}


#' Delete a network
#' @param ndexcon object of class NDEXConnection
#' @param network_id UUID of the network
#' @section REST query:
#' @export
ndex.delete.network <- function(ndexcon, nuuid){
  api = ndex.helper.getApi(ndexcon, 'network$delete')
  route <- ndex.helper.encodeParams(api$url, api$params, nuuid)
  response = ndex_rest_DELETE(ndexcon, route, raw=T)
	return(NULL)
}