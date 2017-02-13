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
##   Aspects and Metadata
##
####################################################


#' Get Network CX Metadata Collection
#' 
#' This function retrieves the (aspect) meta-data of the network identified by the supplied network UUID string.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @return metadata as list: consistencyGroup, elementCount, lastUpdate, name, properties, version and idCounter
#' @section REST query:
#' GET: ndex.api.config$api$network$aspect$getMetaData
#' @note Compatible to NDEx server version 1.3 and 2.0, but doesn't work for version 1.3
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' ndex.network.get.metadata(ndexcon,pws[1,"externalId"]) }
#' @export
ndex.network.get.metadata <- function(ndexcon, network_id){
	api = ndex.helper.getApi(ndexcon, 'network$aspect$getMetaData')
	route <- ndex.helper.encodeParams(api$url, api$params, network_id)
	
	response = ndex_rest_GET(ndexcon, route)
	return(response)
}


#' Get the Metadata Associated with a Network UUID
#' 
#' This function retrieves the metadata associated with the supplied network UUID.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @return metadata for an aspect as list: consistencyGroup, elementCount, lastUpdate, data, name, properties, version and idCounter
#' @section REST query:
#' GET: ndex.api.config$api$network$aspect$getMetaDataByName
#' @note Compatible to NDEx server version 2.0
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' ndex.network.aspect.get.metaData(ndexcon,pws[1,"externalId"]) }
#' @export
ndex.network.aspect.get.metaData <- function(ndexcon, network_id, aspect){
	api = ndex.helper.getApi(ndexcon, 'network$aspect$getMetaDataByName')
	route <- ndex.helper.encodeParams(api$url, api$params, c(network_id, aspect))
	
	response = ndex_rest_GET(ndexcon, route)
	return(response)
}

#' Get a Network Aspect As CX
#' @export
ndex.network.get.aspect <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}


#' Update an Aspect of a Network
#' @export
ndex.network.update.aspect <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}
