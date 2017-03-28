################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 24.03.2017 by Auer
##     
## Description:
##   Functions for Creating, updating and deleting groups, as well as managing group memberships of users and networks
##
## Note:
##   API and functions are only compatible to NDEx server version 2.0
################################################################################

####################################################
## 
##   NDEx Groups functions
##
####################################################
##   CRUD functions for groups
####################################################

#' Get a Group
#' 
#' @param ndexcon object of class NDEXConnection
#' @param groupId character; unique ID (UUID) of the group
#' 
#' @return list of properties describing the group (externalId, emailAddress, website, etc.). Throws error (404) if group isn't found!
#' 
#' @section REST query:
#' GET: ndex.api.config$api$group$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' groupId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' group = ndex.get.group(ndexcon, groupId)
#' }
#' @export
ndex.get.group <- function(ndexcon, groupId) {
    api = ndex.helper.getApi(ndexcon, 'group$get')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


#' Create Group
#' 
#' Create a group owned by the authenticated user based on the supplied group JSON object.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param groupName character; name of the new graoup
#' @param image character (optional); URL of the account owner's image
#' @param website character (optional); URL of the account owner's web site
#' @param description character (optional); Short description of this user
#' @param properties list (optional); additional properties for the group
#'
#' @return url (including the UUID) of the newly created group
#' 
#' @section REST query:
#' POST: ndex.api.config$api$group$create
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' groupId = ndex.create.group(ndexcon, 'SomeGroupName')
#' ## [1] "http://public.ndexbio.org/v2/group/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' 
#' groupId = ndex.create.group(ndexcon, 'SomeGroupName', image='http://bit.ly/1M3NoQZ', website='www.gidf.com', description='A very special group..')
#' }
#' @export
ndex.create.group <- function(ndexcon, groupName, image, website, description, properties) {
    if(missing(groupName)) stop('ndex.create.group: Group name is required!')
    
    data = list(groupName=groupName)
    
    if(!missing(image) && !is.null(image)) data$image = image
    if(!missing(website) && !is.null(website)) data$website = website
    if(!missing(description) && !is.null(description)) data$description = description
    if(!missing(properties) && !is.null(properties)){
        data$properties = NA
        data$properties = as.list(data$properties)
        data$properties = properties
    }
    
    api = ndex.helper.getApi(ndexcon, 'group$create')
    route <- ndex.helper.encodeParams(api$url, api$params)
    data <- jsonlite::toJSON(data, auto_unbox = TRUE)
    
    response = ndex_rest_POST(ndexcon, route, data, raw=T)
    return(response)
}


#' Delete Group
#' 
#' Delete the group specified by groupId
#' 
#' @param ndexcon object of class NDEXConnection
#' @param groupId character; unique ID (UUID) of the group
#' 
#' @return NULL if successfull, else an error is thrown
#' 
#' @section REST query:
#' DELETE: ndex.api.config$api$group$delete
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' groupId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' ndexr.delete.group(ndexcon,groupId)
#' }
#' @export
ndex.delete.group <- function(ndexcon, groupId) {
    api = ndex.helper.getApi(ndexcon, 'group$delete')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId)
    response = ndex_rest_DELETE(ndexcon, route, raw=T)
    return(NULL)
}


#' Update Group
#' 
#' Updates the group based on the data.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param groupId character; unique ID (UUID) of the group
#' @param groupName character; name of the new graoup
#' @param image character (optional); URL of the account owner's image.
#' @param website character (optional); URL of the account owner's web site
#' @param description character (optional); Short description of this user.
#' @param properties list (optional); additional properties for the group
#'
#' @return Empty string ("") on success, else error
#' 
#' @section REST query:
#' PUT: ndex.api.config$api$user$update
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' groupId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' ndex.update.group(ndexcon, groupId, description='A really nice group!')
#' }
#' @export
ndex.update.group <- function(ndexcon, groupId, groupName, image, website, description, properties) {
    if(missing(groupId)) stop('ndex.update.group: Group UUID is required!')
    
    data = list()
    
    if(!missing(groupName) && !is.null(groupName)) data$groupName = groupName
    if(!missing(image) && !is.null(image)) data$image = image
    if(!missing(website) && !is.null(website)) data$website = website
    if(!missing(description) && !is.null(description)) data$description = description
    if(!missing(properties) && !is.null(properties)){
        data$properties = NA
        data$properties = as.list(data$properties)
        data$properties = properties
    }
    
    api = ndex.helper.getApi(ndexcon, 'group$update')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId)
    data <- jsonlite::toJSON(data, auto_unbox = TRUE, null='null')
    
    response = ndex_rest_PUT(ndexcon, route, data, raw=T)
    return(response)
}


####################################################
##   Manage group memberships
####################################################

#' Add or Update a Group Member
#' 
#' Updates the membership corresponding to the GroupMembership type specified in the URL parameter.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param groupId character; unique ID (UUID) of the group
#' @param userId character; unique ID (UUID) of the user
#' @param type character (optional)("GROUPADMIN"|"MEMBER")(default: "MEMBER"); Type of group membership 
#'
#' @return Empty string ("") on success, else error
#' 
#' @section REST query:
#' PUT: ndex.api.config$api$user$membership$update
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' groupId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' userId = "ffffffff-gggg-hhhh-iiii-jjjjjjjjjjjj"
#' ndex.group.set.membership(ndexcon, groupId, userId)
#' ndex.group.set.membership(ndexcon, groupId, userId, type='MEMBER')    ## same as before
#' ndex.group.set.membership(ndexcon, groupId, userId, type='GROUPADMIN')
#' }
#' @export
ndex.group.set.membership <- function(ndexcon, groupId, userId, type='MEMBER') {
    if(missing(groupId)) stop('ndex.group.set.membership: Group UUID is required!')
    if(missing(userId)) stop('ndex.group.set.membership: User UUID is required!')
    
    api = ndex.helper.getApi(ndexcon, 'group$membership$update')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId, user=userId, type=type)
    
    response = ndex_rest_PUT(ndexcon, route, data=NULL, raw=T)
    return(response)
}


#' Remove a Group Member
#' 
#' Removes the member from the group
#' 
#' @param ndexcon object of class NDEXConnection
#' @param groupId character; unique ID (UUID) of the group
#' @param userId character; unique ID (UUID) of the user
#'
#' @return Empty string ("") on success, else error
#' 
#' @section REST query:
#' DELETE: ndex.api.config$api$user$membership$delete
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' groupId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' userId = "ffffffff-gggg-hhhh-iiii-jjjjjjjjjjjj"
#' ndex.group.delete.membership(ndexcon, groupId, userId)
#' }
#' @export
ndex.group.delete.membership <- function(ndexcon, groupId, userId) {
    if(missing(groupId)) stop('ndex.group.delete.membership: Group UUID is required!')
    if(missing(userId)) stop('ndex.group.delete.membership: User UUID is required!')
    
    api = ndex.helper.getApi(ndexcon, 'group$membership$delete')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId, user=userId)
    
    response = ndex_rest_DELETE(ndexcon, route, raw=T)
    return(NULL)
}

#' Get Members of a Group
#'  
#' @param ndexcon object of class NDEXConnection
#' @param groupId character; unique ID (UUID) of the group
#' @param type character (optional); constrains the type of the returned membership. If not set (or NULL), all permission types will be returned. 
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return List of permissions of that group or empty object
#' 
#' @section REST query:
#' GET: ndex.api.config$api$group$membership$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' groupId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' users = ndex.group.list.users (ndexcon, groupId)
#' users = ndex.group.list.users (ndexcon, groupId, type='ADMIN', start=0, size=10)
#' }
#' @export
ndex.group.list.users <- function(ndexcon, groupId, type=NULL, start=NULL, size=NULL) {
    api = ndex.helper.getApi(ndexcon, 'group$user$list')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId, type=type, start=start, size=size)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


####################################################
##   Network permissions
####################################################

#' Get Network Permissions of a Group
#'  
#' @param ndexcon object of class NDEXConnection
#' @param groupId character; unique ID (UUID) of the group
#' @param permission character (optional) ("WRITE"|"READ) (default: "READ"); constrains the type of the returned permission.
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return List of network permissions of that group or empty object
#' 
#' @section REST query:
#' GET: ndex.api.config$api$group$network$list
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples
#' \dontrun{
#' groupId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' networks = ndex.group.list.networks(ndexcon, groupId)
#' networks = ndex.group.list.networks(ndexcon, groupId, permission='READ', start=0, size=10)
#' }
#' @export
ndex.group.list.networks <- function(ndexcon, groupId, permission=NULL, start=NULL, size=NULL) {
    api = ndex.helper.getApi(ndexcon, 'group$network$list')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId, permission=permission, start=start, size=size)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}

#' Get Group Permission for a Specific Network
#'  
#' @param ndexcon object of class NDEXConnection
#' @param groupId character; unique ID (UUID) of the group
#' @param networkId character; unique ID (UUID) of the network
#' 
#' @return Network permissions of that group or empty object
#' 
#' @section REST query:
#' GET: ndex.api.config$api$group$network$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' groupId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' networkId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' group = ndex.group.get.network(ndexcon, groupId, networkId)
#' }
#' @export
ndex.group.get.network <- function(ndexcon, groupId, networkId) {
    api = ndex.helper.getApi(ndexcon, 'group$network$get')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId, network=networkId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}
