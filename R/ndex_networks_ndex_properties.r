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
##    Contains functions to search and retrieve networks
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
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param networkId unique ID of the network
#' @param type character ("user"|"group"); specifies whether user or group permissions should be returned
#' @param permission character (optional)("READ"|"WRITE"|"ADMIN"); constrains the type of the returned membership. If not set (or NULL), all permission types will be returned. 
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return data.frame containing user or group UUIDs and the highest permission assigned to that user or group
#' 
#' @section REST query:
#' GET: ndex.conf$api$network$permission$get
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @note In version 1.3 the function only returns user permissions and differs in the returned data (more columns)!
#' @note Requires an authorized user! (ndex.connect with credentials)
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex.connect('MyAccountName', 'MyPassword')
#' ## Find one of your networks and get its UUID
#' # networks = ndex.find.networks(ndexcon, accountName='MyAccountName')
#' # networkId = networks[1,"externalId"]
#' ## Get the permissions
#' # permissions = ndex.network.get.permission(ndexcon, networkId, 'user')
#' ## Version 2.0:
#' ## names(permission)
#' ## [1] "memberUUID" "permission"
#' ## Version 1.3:
#' ## names(permission)
#' ## [1] "membershipType"    "memberUUID"  "resourceUUID"      
#' ## [4] "memberAccountName" "permissions" "resourceName" 
#' # permissions = ndex.network.get.permission(ndexcon, networkId, 'user', NULL)    # same as previous
#' # permissions = ndex.network.get.permission(ndexcon, networkId, 'user', 'READ', 0, 10)
#' # permissions = ndex.network.get.permission(ndexcon, networkId, 'group')
#' NULL
#' @export
ndex.network.get.permission <- function(ndexcon, networkId, type, permission, start, size){
    
    if (missing(type)) stop('ndex.network.get.permission: parameter "type" not specified!')
    if (!type %in% c('user', 'group')) stop(paste0('ndex.network.get.permission: parameter "type" must have either "user" or "group" as value, but is "',type,'"!'))
    if (missing(permission)) permission = NULL    
    if ((!is.null(permission))&&(!permission %in% c('READ','WRITE','ADMIN'))) stop(paste0('ndex.network.get.permission: parameter "permission" must have either "READ", "WRITE", "ADMIN" as value or be NULL, but is "',permission,'"!'))
    if (missing(start)) start = NULL
    if (missing(size)) size = NULL
    
    api = ndex.helper.getApi(ndexcon, 'network$permission$get')
    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId, type=type, permission=permission, start=start, size=size)
    
    response <- ndex_rest_GET(ndexcon, route=route)
    
    if('list' %in% class(response)){
        if(length(response)==0) response = NULL
        else{
            permission = unlist(response)
            if(is.null(permission)) permission = NULL
            response = data.frame(user=names(response), permission=permission, stringsAsFactors=FALSE, row.names=NULL)
            names(response) = c(ifelse(type=='user', 'memberUUID', 'groupUUID'), 'permission')
        }
    }
    
    return(response)
}


#' Update Network Permission        
#' 
#' Updates the permission of a user specified by userid or group specified by groupid for the network
#' 
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param networkId unique ID of the network
#' @param user character (optional); uuid of the user. Only either user or group may be set!
#' @param group character (optional); uuid of the group. Only either user or group may be set!
#' @param permission character (optional)("READ"|"WRITE"|"ADMIN"); type of permission to be given. If granted admin permission, the current admin loses the admin status.
#' 
#' @return 1 integer on success, 0 if user/group allready has this permissions on the network
#' @note Requires an authorized user! (ndex.connect with credentials)
#' 
#' @section REST query:
#' GET: ndex.conf$api$network$permission$update
#' @note Compatible to NDEx server version 1.3 and 2.0, but doesn't work for version 1.3
#' @note In version 1.3 the function only works for user permissions!
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex.connect('MyAccountName', 'MyPassword')
#' ## Find one of your networks and get its UUID
#' # networks = ndex.find.networks(ndexcon, accountName='MyAccountName')
#' # networkId = networks[1,"externalId"]
#' ## Get the UUID for a user and group
#' # someUserUuid = "uuuuuuuu-ssss-eeee-rrrr-111111111111"
#' # someGroupUuid = "ggggggg-rrrr-oooo-uuuu-pppppppppppp"
#' ## Change the permissions
#' # ndex.network.update.permission(ndexcon, networkId, user=someUserUuid, 'WRITE')
#' # ndex.network.update.permission(ndexcon, networkId, group=someGroupUuid, 'READ')
#' ## Set a new admin (lose own admin status)
#' # ndex.network.update.permission(ndexcon, networkId, user=someUserUuid, 'ADMIN')
#' NULL
#' @export
ndex.network.update.permission <- function(ndexcon, networkId, user=NULL, group=NULL, permission){
    
    if (is.null(user) && is.null(group)) stop(paste0('ndex.network.update.permission: Neither a user nor a group uuid has been set, but one is required!'))
    if ((!is.null(user))&&(!is.null(group))) stop(paste0('ndex.network.update.permission: A user and a group uuid has been set, but one is allowed!'))
    if (missing(permission)) permission = NULL    
    if ((is.null(permission))||(!permission %in% c('READ','WRITE','ADMIN'))) stop(paste0('ndex.network.update.permission: parameter "permission" must have either "READ", "WRITE" or "ADMIN" as value, but is "',ifelse(is.null(permission),'NULL',permission),'"!'))
    
    api = ndex.helper.getApi(ndexcon, 'network$permission$update')
    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId, user=user, group=group, permission=permission)
    
    # different REST methods for the different versions
    if(ndexcon$ndexConf$version=='1.3'){
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
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param networkId unique ID of the network
#' @param user character (optional); uuid of the user. Only either user or group may be set!
#' @param group character (optional); uuid of the group. Only either user or group may be set!
#' 
#' @return 1 integer on success, 0 if user/group allready has no permissions on the network
#' @note Requires an authorized user! (ndex.connect with credentials)
#' 
#' @section REST query:
#' GET: ndex.conf$api$network$permission$delete
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @note In version 1.3 the function only works for user permissions!
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex.connect('MyAccountName', 'MyPassword')
#' ## Find one of your networks and get its UUID
#' # networks = ndex.find.networks(ndexcon, accountName='MyAccountName')
#' # networkId = networks[1,"externalId"]
#' ## Get the UUID for a user and group
#' # someUserUuid = "uuuuuuuu-ssss-eeee-rrrr-111111111111"
#' # someGroupUuid = "ggggggg-rrrr-oooo-uuuu-pppppppppppp"
#' ## Delete the permissions
#' #ndex.network.delete.permission(ndexcon, networkId, user=someUserUuid)    # returns 1
#' #ndex.network.delete.permission(ndexcon, networkId, user=someUserUuid)    # returns 0, because user already lost permission on network
#' #ndex.network.delete.permission(ndexcon, networkId, group=someGroupUuid)
#' NULL
#' @export
ndex.network.delete.permission <- function(ndexcon, networkId, user=NULL, group=NULL){
    
    if (is.null(user) && is.null(group)) stop(paste0('nndex.network.delete.permission: Neither a user nor a group uuid has been set, but one is required!'))
    if ((!is.null(user))&&(!is.null(group))) stop(paste0('ndex.network.delete.permission: A user and a group uuid has been set, but one is allowed!'))
    
    api = ndex.helper.getApi(ndexcon, 'network$permission$delete')
    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId, user=user, group=group)
    
    response <- ndex_rest_DELETE(ndexcon, route=route)
    
    return(response)
}

####################################################
## Network System Properties
####################################################

#' Set Network System Properties    
#' 
#' Network System properties are the properties that describe the network's status in a particular NDEx server but that are not part of the corresponding CX network object.
#' 
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param networkId unique ID of the network
#' @param readOnly boolean (optional); Sets the network to only readable. At least one of readOnly, visibility or showcase have to be set!
#' @param visibility character (optional) ('PUBLIC'|'PRIVATE'); Sets the network to only readable. At least one of readOnly, visibility or showcase have to be set!
#' @param showcase boolean (optional); Authenticated user can use this property to control whether this network will display in his or her home page. Caller will receive an error if the user does not have explicit permission to that network. At least one of readOnly, visibility or showcase have to be set!
#' 
#' @return NULL on success; Error else
#' @note Requires an authorized user! (ndex.connect with credentials)
#' 
#' @section REST query:
#' GET: ndex.conf$api$network$systemproperties$set
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @note In version 1.3 only the parameter readOnly is supported
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex.connect('MyAccountName', 'MyPassword')
#' ## Find one of your networks and get its UUID
#' # networks = ndex.find.networks(ndexcon, accountName='MyAccountName')
#' # networkId = networks[1,"externalId"]
#' ## Set network system properties
#' # ndex.network.set.systemProperties(ndexcon, networkId, readOnly=TRUE)
#' # ndex.network.set.systemProperties(ndexcon, networkId, visibility="PUBLIC")
#' # ndex.network.set.systemProperties(ndexcon, networkId, showcase=TRUE)
#' # ndex.network.set.systemProperties(ndexcon, networkId, readOnly=FALSE, visibility="PRIVATE", showcase=FALSE)
#' NULL
#' @export
ndex.network.set.systemProperties <- function(ndexcon, networkId, readOnly=NULL, visibility=NULL, showcase=NULL){
    
    if (is.null(readOnly) && is.null(visibility) && is.null(showcase)) stop(paste0('ndex.network.set.systemProperties: Neither readOnly, visibility nor showcase has been set, but at least one is required!'))
    
    api = ndex.helper.getApi(ndexcon, 'network$systemproperties$set')
    
    data <- list()
    data$readOnly=readOnly
    data$visibility=visibility
    data$showcase=showcase
    
    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId, readOnly=readOnly)

    if(ndexcon$ndexConf$version=='1.3'){
        if (is.null(readOnly)) stop(paste0('ndex.network.set.systemProperties: Parameter readOnly is not set! (Version 1.3 only supports the "readOnly" system property)'))
        data = NULL    
    }else{
        data = jsonlite::toJSON(data, auto_unbox=TRUE)
    }
    
    response = ndex_rest_PUT(ndexcon, route, data, raw=TRUE)
    return(NULL)
}


####################################################
## Network Samples
####################################################

## Get Network Sample
## 
## @param ndexcon object of class NDEXConnection link{ndex.connect}
## @param networkId character; unique ID (UUID) of the network
## 
## @return Network sample            
#ndex.network.get.samples <- function(ndexcon, networkId){    # TODO! : Implement!
#    
#}
#
#
## Set Sample Network    
## 
## @param ndexcon object of class NDEXConnection link{ndex.connect}
## @param networkId character; unique ID (UUID) of the network
## 
## @return NULL on success        
#ndex.network.set.samples <- function(ndexcon, networkId){    # TODO! : Implement!
#    
#}


####################################################
## Network Profile and Properties
####################################################

#' Update Network Profile    
#' 
#' Updates the profile information of the network. Any profile attributes specified will be updated but attributes that are not specified will have no effect - omission of an attribute does not mean deletion of that attribute.
#' 
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param networkId unique ID of the network
#' @param name character (optional); Changes the name the network. At least one of name, description or version have to be set!
#' @param description character (optional); Changes the description the network. At least one of name, description or version have to be set!
#' @param version character (optional); Changes the version the network. At least one of name, description or version have to be set!
#' 
#' @return NULL on success; Error else
#' @note Requires an authorized user! (ndex.connect with credentials)
#' 
#' @section REST query:
#' GET: ndex.conf$api$network$profile$update
#' @note Compatible to NDEx server version 1.3 and 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex.connect('MyAccountName', 'MyPassword')
#' ## Find one of your networks and get its UUID
#' # networks = ndex.find.networks(ndexcon, accountName='MyAccountName')
#' # networkId = networks[1,"externalId"]
#' ## Update network profile
#' # ndex.network.update.profile(ndexcon, networkId, name="Some fancy name for the network")
#' # ndex.network.update.profile(ndexcon, networkId, description="Description of the network")
#' # ndex.network.update.profile(ndexcon, networkId, version="1.2.3.4")
#' # ndex.network.update.profile(ndexcon, networkId, name="Special test network", description="Nothing to see here", version="1.3")
#' NULL
#' @export
ndex.network.update.profile <- function(ndexcon, networkId, name=NULL, description=NULL, version=NULL){    # TODO! : Implement!
    if (is.null(name) && is.null(description) && is.null(version)) stop(paste0('ndex.network.update.profile: Neither name, description nor version has been set, but at least one is required!'))
    
    api = ndex.helper.getApi(ndexcon, 'network$profile$update')
    
    data <- list()
    data$name=name
    data$description=description
    data$version=version
    
    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId)
    data = jsonlite::toJSON(data, auto_unbox=TRUE)
    
    if(ndexcon$ndexConf$version=='1.3'){
        response = ndex_rest_POST(ndexcon, route, data, raw=TRUE)
    }else{
        response = ndex_rest_PUT(ndexcon, route, data, raw=TRUE)
    }
    return(NULL)
}


## Set Network Properties
## 
## @param ndexcon object of class NDEXConnection link{ndex.connect}
## @param networkId character; unique ID (UUID) of the network
## 
## @return NULL on success        
#ndex.network.set.properties <- function(ndexcon, networkId){    # TODO! : Implement!
#    
#}