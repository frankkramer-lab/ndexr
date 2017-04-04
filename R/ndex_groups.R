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
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param searchString string by which to search
#' @param start integer (optional); specifies that the result is the nth page of the requested data. The default value is 0
#' @param size integer (optional); specifies the number of data items in each page. The default value is 100
#' 
#' @return Data frame with group information; NULL if no groups are found.
#' 
#' @section REST query:
#' GET: ndex.conf$api$search$user
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @note Search strings may be structured
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' ## Find a group
#' groups = ndex.find.groups(ndexcon,"Ideker Lab")
#' names(groups)
#' ## [1] "properties"       "groupName"        "image"            "website"          "description"     
#' ## [6] "externalId"       "isDeleted"        "modificationTime" "creationTime" 
#' @export
ndex.find.groups <- function(ndexcon, searchString="", start, size){
    
    if (missing(start)) start = NULL
    if (missing(size)) size = NULL
    
    ##Form JSON to post
    query = list(searchString=searchString)
    query <- jsonlite::toJSON(query, auto_unbox = TRUE)
    
    ##Form route
    api = ndex.helper.getApi(ndexcon, 'search$group')
    route <- ndex.helper.encodeParams(api$url, api$params, start=start, size=size)
    
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
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param groupId character; unique ID (UUID) of the group
#' 
#' @return list of properties describing the group (externalId, emailAddress, website, etc.). Throws error (404) if group isn't found!
#' 
#' @section REST query:
#' GET: ndex.conf$api$group$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' ## Find a group
#' groups = ndex.find.groups(ndexcon,"Ideker Lab")
#' groupId = groups[1,"externalId"]
#' ## Get group information
#' group = ndex.get.group(ndexcon, groupId)
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
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param groupName character; name of the new graoup
#' @param image character (optional); URL of the account owner's image
#' @param website character (optional); URL of the account owner's web site
#' @param description character (optional); Short description of this user
#' @param properties list (optional); additional properties for the group
#'
#' @return url (including the UUID) of the newly created group
#' @note Requires an authorized user! (ndex.connect with credentials)
#' 
#' @section REST query:
#' POST: ndex.conf$api$group$create
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex.connect('MyAccountName', 'MyPassword')
#' # groupURL = ndex.create.group(ndexcon, 'SomeGroupName')
#' ## [1] "http://public.ndexbio.org/v2/group/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' # groupURL = ndex.create.group(ndexcon, 'SomeGroupName', image='http://bit.ly/1M3NoQZ', website='www.gidf.com', description='A very special group..')
#' NULL
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
    
    response = ndex_rest_POST(ndexcon, route, data, raw=TRUE)
    return(response)
}


#' Delete Group
#' 
#' Delete the group specified by groupId
#' 
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param groupId character; unique ID (UUID) of the group
#' 
#' @return NULL if successfull, else an error is thrown
#' @note Requires an authorized user! (ndex.connect with credentials)
#' 
#' @section REST query:
#' DELETE: ndex.conf$api$group$delete
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex.connect('MyAccountName', 'MyPassword')
#' ## Find user and get its id
#' # user = ndex.find.user.byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## Find the user's groups and get one group id
#' # groups = ndex.user.list.groups(ndexcon, userId)
#' # groupId = groups[1,"externalId"]
#' #ndexr.delete.group(ndexcon,groupId)
#' NULL
#' @export
ndex.delete.group <- function(ndexcon, groupId) {
    api = ndex.helper.getApi(ndexcon, 'group$delete')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId)
    response = ndex_rest_DELETE(ndexcon, route, raw=TRUE)
    return(NULL)
}


#' Update Group
#' 
#' Updates the group based on the data.
#' 
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param groupName character; name of the new graoup
#' @param image character (optional); URL of the account owner's image.
#' @param website character (optional); URL of the account owner's web site
#' @param description character (optional); Short description of this user.
#' @param properties list (optional); additional properties for the group
#'
#' @return Empty string ("") on success, else error
#' @note Requires an authorized user! (ndex.connect with credentials)
#' 
#' @section REST query:
#' PUT: ndex.conf$api$user$update
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex.connect('MyAccountName', 'MyPassword')
#' ## Find user and get its id
#' # user = ndex.find.user.byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## Find the user's groups and get one group id
#' # groups = ndex.user.list.groups(ndexcon, userId)
#' # groupId = groups[1,"externalId"]
#' ## Update the group
#' # ndex.update.group(ndexcon, groupId, description='A really nice group!')
#' NULL
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
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param userId character; unique ID (UUID) of the user
#' @param type character (optional)("GROUPADMIN"|"MEMBER")(default: "MEMBER"); Type of group membership 
#'
#' @return Empty string ("") on success, else error
#' @note Requires an authorized user! (ndex.connect with credentials)
#' 
#' @section REST query:
#' PUT: ndex.conf$api$user$membership$update
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex.connect('MyAccountName', 'MyPassword')
#' ## Find user and get own id
#' # user = ndex.find.user.byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## Find own groups and get one group id
#' # groups = ndex.user.list.groups(ndexcon, userId)
#' # groupId = groups[1,"externalId"]
#' ## Find an other user and get the id
#' # user = ndex.find.user.byName(ndexcon, 'SomeOtherAccountName')
#' # userId = user$externalId
#' ## Add other user to the group
#' # ndex.group.set.membership(ndexcon, groupId, userId)
#' ## Update other user's group permission
#' # ndex.group.set.membership(ndexcon, groupId, userId, type='MEMBER')    ## same as before
#' ## Make other user to group admin (lose own admin permission)
#' # ndex.group.set.membership(ndexcon, groupId, userId, type='GROUPADMIN')
#' NULL
#' @export
ndex.group.set.membership <- function(ndexcon, groupId, userId, type='MEMBER') {
    if(missing(groupId)) stop('ndex.group.set.membership: Group UUID is required!')
    if(missing(userId)) stop('ndex.group.set.membership: User UUID is required!')
    
    api = ndex.helper.getApi(ndexcon, 'group$membership$update')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId, user=userId, type=type)
    
    response = ndex_rest_PUT(ndexcon, route, data=NULL, raw=TRUE)
    return(response)
}


#' Remove a Group Member
#' 
#' Removes the member from the group
#' 
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param userId character; unique ID (UUID) of the user
#'
#' @return Empty string ("") on success, else error
#' @note Requires an authorized user! (ndex.connect with credentials)
#' 
#' @section REST query:
#' DELETE: ndex.conf$api$user$membership$delete
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex.connect('MyAccountName', 'MyPassword')
#' ## Find user and get own id
#' # user = ndex.find.user.byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## Find own groups and get one group id
#' # groups = ndex.user.list.groups(ndexcon, userId)
#' # groupId = groups[1,"externalId"]
#' ## Find an other user of the group and get the id
#' # users = ndex.group.list.users(ndexcon, groupId)
#' ## Choose one user 
#' # userId = users[1,"externalId"]
#' ## Remove user from the group
#' # ndex.group.delete.membership(ndexcon, groupId, userId)
#' NULL
#' @export
ndex.group.delete.membership <- function(ndexcon, groupId, userId) {
    if(missing(groupId)) stop('ndex.group.delete.membership: Group UUID is required!')
    if(missing(userId)) stop('ndex.group.delete.membership: User UUID is required!')
    
    api = ndex.helper.getApi(ndexcon, 'group$membership$delete')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId, user=userId)
    
    response = ndex_rest_DELETE(ndexcon, route, raw=TRUE)
    return(NULL)
}

#' Get Members of a Group
#'  
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param type character (optional); constrains the type of the returned membership. If not set (or NULL), all permission types will be returned. 
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return List of permissions of that group or empty object
#' 
#' @section REST query:
#' GET: ndex.conf$api$group$membership$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' ## Find a group
#' groups = ndex.find.groups(ndexcon,"Ideker Lab")
#' groupId = groups[1,"externalId"]
#' ## Find other users of the group
#' # users = ndex.group.list.users(ndexcon, groupId)
#' # users = ndex.group.list.users (ndexcon, groupId, type='ADMIN', start=0, size=10)
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
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param permission character (optional) ("WRITE"|"READ) (default: "READ"); constrains the type of the returned permission.
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return List of network permissions of that group or empty object
#' 
#' @section REST query:
#' GET: ndex.conf$api$group$network$list
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' ## Find a group
#' groups = ndex.find.groups(ndexcon,"Ideker Lab")
#' groupId = groups[1,"externalId"]
#' ## List networks of the group
#' networks = ndex.group.list.networks(ndexcon, groupId)
#' networks = ndex.group.list.networks(ndexcon, groupId, permission='READ', start=0, size=10)
#' @export
ndex.group.list.networks <- function(ndexcon, groupId, permission=NULL, start=NULL, size=NULL) {
    api = ndex.helper.getApi(ndexcon, 'group$network$list')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId, permission=permission, start=start, size=size)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}

#' Get Group Permission for a Specific Network
#'  
#' @param ndexcon object of class NDEXConnection \link{ndex.connect}
#' @param groupId character; unique ID (UUID) of the group
#' @param networkId character; unique ID (UUID) of the network
#' 
#' @return Network permissions of that group or empty object
#' 
#' @section REST query:
#' GET: ndex.conf$api$group$network$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' ## Find a group
#' groups = ndex.find.groups(ndexcon,"Ideker Lab")
#' groupId = groups[1,"externalId"]
#' ## List networks of the group
#' networks = ndex.group.list.networks(ndexcon, groupId)
#' networkId = names(networks)[1]
#' ## Get group's permission to the network
#' #group = ndex.group.network.get.permission(ndexcon, groupId, networkId)
#' @export
ndex.group.network.get.permission <- function(ndexcon, groupId, networkId) {
    api = ndex.helper.getApi(ndexcon, 'group$network$get')
    route <- ndex.helper.encodeParams(api$url, api$params, group=groupId, network=networkId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}
