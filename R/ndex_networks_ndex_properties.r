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
#' 
#' This function retrieves the user or group permissions for a network
#' 
#' @param ndexcon object of class NDEXConnection
#' @param nuuid unique ID of the network
#' @param type character ("user"|"group"); specifies whether user or group permissions should be returned
#' @param permission character (optional)("READ"|"WRITE"|"ADMIN"); constrains the type of the returned membership. If not set (or NULL), all permission types will be returned. 
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' @return data.frame containing user or group UUIDs and the highest permission assigned to that user or group
#' @section REST query:
#' GET: ndex.api.config$api$network$permission$get
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @note In version 1.3 the function only returns user permissions and differs in the returned data (more columns)!
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect('MyAccountName', 'MyPassword', verbose=T)
#' networks = ndex.find.networks(ndexcon,"p53")
#' nuuid = networks[1,"externalId"]
#' ## get the permissions
#' permissions = ndex.network.get.permission(ndexcon, nuuid, 'user')
#' permissions = ndex.network.get.permission(ndexcon, nuuid, 'user', NULL)	# same as previous
#' permissions = ndex.network.get.permission(ndexcon, nuuid, 'user', 'READ', 0, 10)
#' permissions = ndex.network.get.permission(ndexcon, nuuid, 'group')
#' }
#' @export
ndex.network.get.permission <- function(ndexcon, nuuid, type, permission, start, size){
	
	if (missing(type)) stop('ndex.network.get.permission: parameter "type" not specified!')
	if (!type %in% c('user', 'group')) stop(paste0('ndex.network.get.permission: parameter "type" must have either "user" or "group" as value, but is "',type,'"!'))
	if (missing(permission)) permission = NULL	
	if ((!is.null(permission))&&(!permission %in% c('READ','WRITE','ADMIN'))) stop(paste0('ndex.network.get.permission: parameter "permission" must have either "READ", "WRITE", "ADMIN" as value or be NULL, but is "',permission,'"!'))
	if (missing(start)) start = NULL
	if (missing(size)) size = NULL
	
	api = ndex.helper.getApi(ndexcon, 'network$permission$get')
	route <- ndex.helper.encodeParams(api$url, api$params, network=nuuid, type=type, permission=permission, start=start, size=size)
	
	response <- ndex_rest_GET(ndexcon, route=route)
	
	if('list' %in% class(response)){
		if(length(response)==0) response = NULL
		else{
			permission = unlist(response)
			if(is.null(permission)) permission = NULL
			response = data.frame(user=names(response), permission=permission, stringsAsFactors=F, row.names=NULL)
			names(response) = c(ifelse(type=='user', 'memberUUID', 'groupUUID'), 'permission')
		}
	}
	
	return(response)
}


#' Update Network Permission		
#' 
#' Updates the permission of a user specified by userid or group specified by groupid for the network
#' 
#' @param ndexcon object of class NDEXConnection
#' @param nuuid unique ID of the network
#' @param user character (optional); uuid of the user. Only either user or group may be set!
#' @param group character (optional); uuid of the group. Only either user or group may be set!
#' @param permission character (optional)("READ"|"WRITE"|"ADMIN"); type of permission to be given. If granted admin permission, the current admin loses the admin status.
#' @return 1 integer on success, 0 if user/group allready has this permissions on the network
#' @section REST query:
#' GET: ndex.api.config$api$network$permission$update
#' @note Compatible to NDEx server version 1.3 and 2.0, but doesn't work for version 1.3
#' @note In version 1.3 the function only works for user permissions!
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect('MyAccountName', 'MyPassword', verbose=T)
#' networks = ndex.find.networks(ndexcon,"p53")
#' nuuid = networks[1,"externalId"]
#' ## get the permissions
#' ndex.network.update.permission(ndexcon, nuuid, user=someUserUuid, 'WRITE')
#' ndex.network.update.permission(ndexcon, nuuid, group=someGroupUuid, 'READ')
#' ndex.network.update.permission(ndexcon, nuuid, user=someUserUuid, 'ADMIN')	# sets a new admin
#' }
#' @export
ndex.network.update.permission <- function(ndexcon, nuuid, user=NULL, group=NULL, permission){
	
	if (is.null(user) && is.null(group)) stop(paste0('ndex.network.update.permission: Neither a user nor a group uuid has been set, but one is required!'))
	if ((!is.null(user))&&(!is.null(group))) stop(paste0('ndex.network.update.permission: A user and a group uuid has been set, but one is allowed!'))
	if (missing(permission)) permission = NULL	
	if ((is.null(permission))||(!permission %in% c('READ','WRITE','ADMIN'))) stop(paste0('ndex.network.update.permission: parameter "permission" must have either "READ", "WRITE" or "ADMIN" as value, but is "',ifelse(is.null(permission),'NULL',permission),'"!'))
	
	api = ndex.helper.getApi(ndexcon, 'network$permission$update')
	route <- ndex.helper.encodeParams(api$url, api$params, network=nuuid, user=user, group=group, permission=permission)
	
	# different REST methods for the different versions
	if(ndexcon$apiConfig$version=='1.3'){
		response <- ndex_rest_POST(ndexcon, route=route, data=list(CXNetworkStream = list(permission=permission)))
	}else{
		response <- ndex_rest_PUT(ndexcon, route=route)
	}
	
	return(response)
}


#' Delete Network Permission		
#' 
#' Removes any permission for the network for the user or group specified
#' 
#' @param ndexcon object of class NDEXConnection
#' @param nuuid unique ID of the network
#' @param user character (optional); uuid of the user. Only either user or group may be set!
#' @param group character (optional); uuid of the group. Only either user or group may be set!
#' @return 1 integer on success, 0 if user/group allready has no permissions on the network
#' @section REST query:
#' GET: ndex.api.config$api$network$permission$delete
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect('MyAccountName', 'MyPassword', verbose=T)
#' networks = ndex.find.networks(ndexcon,"p53")
#' nuuid = networks[1,"externalId"]
#' ## get the permissions
#' ndex.network.delete.permission(ndexcon, nuuid, user=someUserUuid)	# returns 1
#' ndex.network.delete.permission(ndexcon, nuuid, user=someUserUuid)	# returns 0, because user already lost permission on network
#' ndex.network.delete.permission(ndexcon, nuuid, group=someGroupUuid)
#' }
#' @export
ndex.network.delete.permission <- function(ndexcon, nuuid, user=NULL, group=NULL){
	
	if (is.null(user) && is.null(group)) stop(paste0('nndex.network.delete.permission: Neither a user nor a group uuid has been set, but one is required!'))
	if ((!is.null(user))&&(!is.null(group))) stop(paste0('ndex.network.delete.permission: A user and a group uuid has been set, but one is allowed!'))
	
	api = ndex.helper.getApi(ndexcon, 'network$permission$delete')
	route <- ndex.helper.encodeParams(api$url, api$params, network=nuuid, user=user, group=group)
	
	response <- ndex_rest_DELETE(ndexcon, route=route)
	
	return(response)
}

####################################################
## Network System Properties
####################################################

#' Set Network System Properties	
#' 
#' Network System properties are the properties that describe the network’s status in a particular NDEx server but that are not part of the corresponding CX network object.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param nuuid unique ID of the network
#' @param readOnly boolean (optional); Sets the network to only readable. At least one of readOnly, visibility or showcase have to be set!
#' @param visibility character (optional) ('PUBLIC'|'PRIVATE'); Sets the network to only readable. At least one of readOnly, visibility or showcase have to be set!
#' @param showcase boolean (optional); Authenticated user can use this property to control whether this network will display in his or her home page. Caller will receive an error if the user does not have explicit permission to that network. At least one of readOnly, visibility or showcase have to be set!
#' @section REST query:
#' GET: ndex.api.config$api$network$systemproperties$set
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @note In version 1.3 only the parameter readOnly is supported
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect('MyAccountName', 'MyPassword', verbose=T)
#' networks = ndex.find.networks(ndexcon,"p53")
#' nuuid = networks[1,"externalId"]
#' ## set network system properties
#' ndex.network.set.systemProperties(ndexcon, nuuid, readOnly=TRUE)
#' ndex.network.set.systemProperties(ndexcon, nuuid, visibility="PUBLIC")
#' ndex.network.set.systemProperties(ndexcon, nuuid, showcase=TRUE)
#' ndex.network.set.systemProperties(ndexcon, nuuid, readOnly=FALSE, visibility="PRIVATE", showcase=FALSE)
#' }
#' @export
ndex.network.set.systemProperties <- function(ndexcon, nuuid, readOnly=NULL, visibility=NULL, showcase=NULL){
	
	if (is.null(readOnly) && is.null(visibility) && is.null(showcase)) stop(paste0('ndex.network.set.systemProperties: Neither readOnly, visibility nor showcase has been set, but at least one is required!'))
	
	api = ndex.helper.getApi(ndexcon, 'network$systemproperties$set')
	
	data <- list()
	data$readOnly=readOnly
	data$visibility=visibility
	data$showcase=showcase
	
	route <- ndex.helper.encodeParams(api$url, api$params, network=nuuid, readOnly=readOnly)

	if(ndexcon$apiConfig$version=='1.3'){
		if (is.null(readOnly)) stop(paste0('ndex.network.set.systemProperties: Parameter readOnly is not set! (Version 1.3 only supports the "readOnly" system property)'))
		data = NULL	
	}else{
		data = jsonlite::toJSON(data, auto_unbox=T)
	}
	
	response = ndex_rest_PUT(ndexcon, route, data, raw=T)
	return(NULL)
}


####################################################
## Network Samples
####################################################

#' Get Network Sample			
#' @export
ndex.network.get.samples <- function(ndexcon, nuuid){	#!!ToDo: Implement!
	
}


#' Set Sample Network			
#' @export
ndex.network.set.samples <- function(ndexcon, nuuid){	#!!ToDo: Implement!
	
}


####################################################
## Network Profiles
####################################################

#' Update Network Profile		
#' @export
ndex.network.update.profile <- function(ndexcon, nuuid){	#!!ToDo: Implement!
	
}


#' Set Network Properties		
#' @export
ndex.network.set.properties <- function(ndexcon, nuuid){	#!!ToDo: Implement!
	
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
#' @param nuuid unique ID of the network
#' @return List of network metadata: ID, name, whether it is public, edge and node count; source and format of network
#' @section REST query:
#' This function runs GET query /network/{networkUUID}/provenance    and returns Provenance
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' ndex.network.get.provenance(ndexcon,pws[1,"externalId"]) }
#' @export
ndex.network.get.provenance <- function(ndexcon, nuuid){	#!!ToDo: Check and update to api 2.0
	route <- paste0("/network/", nuuid,"/provenance")
	response <- ndex_rest_GET(ndexcon, route)
	return(response)
}


#' Set Network Provenance		
#' @export
ndex.network.set.provenance <- function(ndexcon, nuuid){	#!!ToDo: Implement!
	
}
