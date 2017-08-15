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
##   Find groups
####################################################

#' Search groups in NDEx
#' 
#' Returns a SearchResult object which contains an array of Group objects
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param searchString string by which to search
#' @param start integer (optional); specifies that the result is the nth page of the requested data. The default value is 0
#' @param size integer (optional); specifies the number of data items in each page. The default value is 100
#' 
#' @return Data frame with group information; NULL if no groups are found.
#' 
#' @section REST query:
#' GET: ndex_config$api$search$user
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @note Search strings may be structured
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find a group
#' groups = ndex_find_groups(ndexcon,"Ideker Lab")
#' names(groups)
#' ## [1] "properties"       "groupName"        "image"            "website"          "description"     
#' ## [6] "externalId"       "isDeleted"        "modificationTime" "creationTime" 
#' @export
ndex_find_groups <- function(ndexcon, searchString="", start, size){
    
    if (missing(start)) start = NULL
    if (missing(size)) size = NULL
    
    ##Form JSON to post
    query = list(searchString=searchString)
    query <- jsonlite::toJSON(query, auto_unbox = TRUE)
    
    ##Form route
    api = ndex_helper_getApi(ndexcon, 'search$group')
    route <- ndex_helper_encodeParams(api$url, api$params, start=start, size=size)
    
    ##Get a list of SearchResult objects of users
    response <- ndex_rest_POST(ndexcon, route=route, data=query)
    response = response$resultList
    
    if(length(response) > 0){
        return(response)
    } else {
        return(NULL)
    }
}

####################################################
##   CRUD functions for groups
####################################################

#' Get a Group
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param groupId character; unique ID (UUID) of the group
#' 
#' @return list of properties describing the group (externalId, emailAddress, website, etc.). Throws error (404) if group isn't found!
#' 
#' @section REST query:
#' GET: ndex_config$api$group$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find a group
#' groups = ndex_find_groups(ndexcon,"Ideker Lab")
#' groupId = groups[1,"externalId"]
#' ## Get group information
#' group = ndex_get_group(ndexcon, groupId)
#' @export
ndex_get_group <- function(ndexcon, groupId) {
    api = ndex_helper_getApi(ndexcon, 'group$get')
    route <- ndex_helper_encodeParams(api$url, api$params, group=groupId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


#' Create Group
#' 
#' Create a group owned by the authenticated user based on the supplied group JSON object.
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param groupName character; name of the new graoup
#' @param image character (optional); URL of the account owner's image
#' @param website character (optional); URL of the account owner's web site
#' @param description character (optional); Short description of this user
#' @param properties list (optional); additional properties for the group
#'
#' @return url (including the UUID) of the newly created group
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' POST: ndex_config$api$group$create
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' # groupURL = ndex_create_group(ndexcon, 'SomeGroupName')
#' ## [1] "http://public.ndexbio.org/v2/group/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' # groupURL = ndex_create_group(ndexcon, 'SomeGroupName', image='http://bit.ly/1M3NoQZ', website='www.gidf.com', description='A very special group..')
#' NULL
#' @export
ndex_create_group <- function(ndexcon, groupName, image, website, description, properties) {
    if(missing(groupName)) stop('ndex_create_group: Group name is required!')
    
    data = list(groupName=groupName)
    
    if(!missing(image) && !is.null(image)) data$image = image
    if(!missing(website) && !is.null(website)) data$website = website
    if(!missing(description) && !is.null(description)) data$description = description
    if(!missing(properties) && !is.null(properties)){
        data$properties = NA
        data$properties = as.list(data$properties)
        data$properties = properties
    }
    
    api = ndex_helper_getApi(ndexcon, 'group$create')
    route <- ndex_helper_encodeParams(api$url, api$params)
    data <- jsonlite::toJSON(data, auto_unbox = TRUE)
    
    response = ndex_rest_POST(ndexcon, route, data, raw=TRUE)
    return(response)
}


#' Delete Group
#' 
#' Delete the group specified by groupId
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param groupId character; unique ID (UUID) of the group
#' 
#' @return NULL if successfull, else an error is thrown
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' DELETE: ndex_config$api$group$delete
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## Find user and get its id
#' # user = ndex_find_user_byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## Find the user's groups and get one group id
#' # groups = ndex_user_list_groups(ndexcon, userId)
#' # groupId = groups[1,"externalId"]
#' #ndex_delete_group(ndexcon,groupId)
#' NULL
#' @export
ndex_delete_group <- function(ndexcon, groupId) {
    api = ndex_helper_getApi(ndexcon, 'group$delete')
    route <- ndex_helper_encodeParams(api$url, api$params, group=groupId)
    response = ndex_rest_DELETE(ndexcon, route, raw=TRUE)
    return(NULL)
}


#' Update Group
#' 
#' Updates the group based on the data.
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param groupName character; name of the new graoup
#' @param image character (optional); URL of the account owner's image.
#' @param website character (optional); URL of the account owner's web site
#' @param description character (optional); Short description of this user.
#' @param properties list (optional); additional properties for the group
#'
#' @return Empty string ("") on success, else error
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' PUT: ndex_config$api$user$update
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## Find user and get its id
#' # user = ndex_find_user_byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## Find the user's groups and get one group id
#' # groups = ndex_user_list_groups(ndexcon, userId)
#' # groupId = groups[1,"externalId"]
#' ## Update the group
#' # ndex_update_group(ndexcon, groupId, description='A really nice group!')
#' NULL
#' @export
ndex_update_group <- function(ndexcon, groupId, groupName, image, website, description, properties) {
    if(missing(groupId)) stop('ndex_update_group: Group UUID is required!')
    
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
    
    api = ndex_helper_getApi(ndexcon, 'group$update')
    route <- ndex_helper_encodeParams(api$url, api$params, group=groupId)
    data <- jsonlite::toJSON(data, auto_unbox = TRUE, null='null')
    
    response = ndex_rest_PUT(ndexcon, route, data, raw=TRUE)
    return(response)
}


####################################################
##   Manage group memberships
####################################################

#' Add or Update a Group Member
#' 
#' Updates the membership corresponding to the GroupMembership type specified in the URL parameter.
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param userId character; unique ID (UUID) of the user
#' @param type character (optional)("GROUPADMIN"|"MEMBER")(default: "MEMBER"); Type of group membership 
#'
#' @return Empty string ("") on success, else error
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' PUT: ndex_config$api$user$membership$update
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## Find user and get own id
#' # user = ndex_find_user_byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## Find own groups and get one group id
#' # groups = ndex_user_list_groups(ndexcon, userId)
#' # groupId = groups[1,"externalId"]
#' ## Find an other user and get the id
#' # user = ndex_find_user_byName(ndexcon, 'SomeOtherAccountName')
#' # userId = user$externalId
#' ## Add other user to the group
#' # ndex_group_set_membership(ndexcon, groupId, userId)
#' ## Update other user's group permission
#' # ndex_group_set_membership(ndexcon, groupId, userId, type='MEMBER')    ## same as before
#' ## Make other user to group admin (lose own admin permission)
#' # ndex_group_set_membership(ndexcon, groupId, userId, type='GROUPADMIN')
#' NULL
#' @export
ndex_group_set_membership <- function(ndexcon, groupId, userId, type='MEMBER') {
    if(missing(groupId)) stop('ndex_group_set_membership: Group UUID is required!')
    if(missing(userId)) stop('ndex_group_set_membership: User UUID is required!')
    
    api = ndex_helper_getApi(ndexcon, 'group$membership$update')
    route <- ndex_helper_encodeParams(api$url, api$params, group=groupId, user=userId, type=type)
    
    response = ndex_rest_PUT(ndexcon, route, data=NULL, raw=TRUE)
    return(response)
}


#' Remove a Group Member
#' 
#' Removes the member from the group
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param userId character; unique ID (UUID) of the user
#'
#' @return Empty string ("") on success, else error
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' DELETE: ndex_config$api$user$membership$delete
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## Find user and get own id
#' # user = ndex_find_user_byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## Find own groups and get one group id
#' # groups = ndex_user_list_groups(ndexcon, userId)
#' # groupId = groups[1,"externalId"]
#' ## Find an other user of the group and get the id
#' # users = ndex_group_list_users(ndexcon, groupId)
#' ## Choose one user 
#' # userId = users[1,"externalId"]
#' ## Remove user from the group
#' # ndex_group_delete_membership(ndexcon, groupId, userId)
#' NULL
#' @export
ndex_group_delete_membership <- function(ndexcon, groupId, userId) {
    if(missing(groupId)) stop('ndex_group_delete_membership: Group UUID is required!')
    if(missing(userId)) stop('ndex_group_delete_membership: User UUID is required!')
    
    api = ndex_helper_getApi(ndexcon, 'group$membership$delete')
    route <- ndex_helper_encodeParams(api$url, api$params, group=groupId, user=userId)
    
    response = ndex_rest_DELETE(ndexcon, route, raw=TRUE)
    return(NULL)
}

#' Get Members of a Group
#'  
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param type character (optional); constrains the type of the returned membership. If not set (or NULL), all permission types will be returned. 
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return List of permissions of that group or empty object
#' 
#' @section REST query:
#' GET: ndex_config$api$group$membership$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find a group
#' groups = ndex_find_groups(ndexcon,"Ideker Lab")
#' groupId = groups[1,"externalId"]
#' ## Find other users of the group
#' # users = ndex_group_list_users(ndexcon, groupId)
#' # users = ndex_group_list_users (ndexcon, groupId, type='ADMIN', start=0, size=10)
#' @export
ndex_group_list_users <- function(ndexcon, groupId, type=NULL, start=NULL, size=NULL) {
    api = ndex_helper_getApi(ndexcon, 'group$user$list')
    route <- ndex_helper_encodeParams(api$url, api$params, group=groupId, type=type, start=start, size=size)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


####################################################
##   Network permissions
####################################################

#' Get Network Permissions of a Group
#'  
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param permission character (optional) ("WRITE"|"READ) (default: "READ"); constrains the type of the returned permission.
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return List of network permissions of that group or empty object
#' 
#' @section REST query:
#' GET: ndex_config$api$group$network$list
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find a group
#' groups = ndex_find_groups(ndexcon,"Ideker Lab")
#' groupId = groups[1,"externalId"]
#' ## List networks of the group
#' networks = ndex_group_list_networks(ndexcon, groupId)
#' networks = ndex_group_list_networks(ndexcon, groupId, permission='READ', start=0, size=10)
#' @export
ndex_group_list_networks <- function(ndexcon, groupId, permission=NULL, start=NULL, size=NULL) {
    api = ndex_helper_getApi(ndexcon, 'group$network$list')
    route <- ndex_helper_encodeParams(api$url, api$params, group=groupId, permission=permission, start=start, size=size)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}

#' Get Group Permission for a Specific Network
#'  
#' @param ndexcon object of class NDExConnection \link{ndex_connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param networkId character; unique ID (UUID) of the network
#' 
#' @return Network permissions of that group or empty object
#' 
#' @section REST query:
#' GET: ndex_config$api$group$network$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find a group
#' groups = ndex_find_groups(ndexcon,"Ideker Lab")
#' groupId = groups[1,"externalId"]
#' ## List networks of the group
#' networks = ndex_group_list_networks(ndexcon, groupId)
#' networkId = names(networks)[1]
#' ## Get group's permission to the network
#' #group = ndex_group_network_get_permission(ndexcon, groupId, networkId)
#' @export
ndex_group_network_get_permission <- function(ndexcon, groupId, networkId) {
    api = ndex_helper_getApi(ndexcon, 'group$network$get')
    route <- ndex_helper_encodeParams(api$url, api$params, group=groupId, network=networkId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}
