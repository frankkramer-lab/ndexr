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
##   NDEx Network properties
##
####################################################
## Network Permissions
####################################################

#' Get All Permissions on a Network	
#' @export
ndex.network.get.permission <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}


#' Update Network Permission		
#' @export
ndex.network.update.permission <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}


#' Delete Network Permission		
#' @export
ndex.network.delete.permission <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}

####################################################
## Network System Properties
####################################################

#' Set Network System Properties	
#' @export
ndex.network.set.systemProperties <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}


####################################################
## Network Samples
####################################################

#' Get Network Sample			
#' @export
ndex.network.get.samples <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}


#' Set Sample Network			
#' @export
ndex.network.set.samples <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}


####################################################
## Network Profiles
####################################################

#' Update Network Profile		
#' @export
ndex.network.update.profile <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}


#' Set Network Properties		
#' @export
ndex.network.set.properties <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}


####################################################
## Network Provenance
## For structure and documentation see:  http://www.home.ndexbio.org/network-provenance-history/
####################################################


#' Get Network Provenance
#' 
#' This function retrieves the provenance of the network identified by the supplied network UUID string.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param network_id unique ID of the network
#' @return List of network metadata: ID, name, whether it is public, edge and node count; source and format of network
#' @section REST query:
#' This function runs GET query /network/{networkUUID}/provenance    and returns Provenance
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' ndex.network.get.provenance(ndexcon,pws[1,"externalId"]) }
#' @export
ndex.network.get.provenance <- function(ndexcon, network_id){	#!!ToDo: Check and update to api 2.0
	route <- paste0("/network/", network_id,"/provenance")
	response <- ndex_rest_GET(ndexcon, route)
	return(response)
}


#' Set Network Provenance		
#' @export
ndex.network.set.provenance <- function(ndexcon, network_id){	#!!ToDo: Implement!
	
}
