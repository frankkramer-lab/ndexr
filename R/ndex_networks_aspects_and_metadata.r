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
#' @export
ndex.network.get.aspects <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}


#' Get the Metadata Associated with a Network UUID
#' 
#' This function retrieves the metadata associated with the supplied network UUID.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @return data.frame listing network metadata: ID, name, whether it is public, edge and node count; source and format of network
#' @section REST query:
#' This function runs GET query /network/{networkId}/metadata    and returns the network metadata as a data.frame
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' ndex.network.aspect.get.metaData(ndexcon,pws[1,"externalId"]) }
#' @export
ndex.network.aspect.get.metaData <- function(ndexcon, network_id){	#!!ToDo: Update and implement!
	route <- paste0("/network/", network_id,"/metadata")
	response <- ndex_rest_GET(ndexcon, route)
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
