################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 23.03.2017 by Auer
##     
## Description:
##   Functions for creating, updating and deleting users, passwords and user permissions
##
## Note:
##   API and functions are only compatible to NDEx server version 2.0
################################################################################


####################################################
## 
##   NDEx User functions
##
####################################################
##   Find users
####################################################

#' Get User By Name
#' 
#' @param ndexcon object of class NDEXConnection
#' @param name name of the user
#' 
#' @return list of properties describing the user (externalId, emailAddress, website, etc.). Throws error (404) if user isn't found!
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$get$byName
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' user = ndex.find.user.byName(ndexcon, 'someAccountName')
#' }
#' @export
ndex.find.user.byName <- function(ndexcon, name) {
    api = ndex.helper.getApi(ndexcon, 'user$get$byName')
    route <- ndex.helper.encodeParams(api$url, api$params, userName=name)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


#' Get User By UUID
#' 
#' @param ndexcon object of class NDEXConnection
#' @param uuuid unique ID of the user
#' 
#' @return list of properties describing the user (externalId, emailAddress, website, etc.). Throws error (404) if user isn't found!
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$get$byId
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' user = ndex.find.user.byName(ndexcon, 'someAccountName')
#' user = ndex.find.user.byId(ndexcon, user$externalId)
#' }
#' @export
ndex.find.user.byId <- function(ndexcon, uuuid) {
    api = ndex.helper.getApi(ndexcon, 'user$get$byId')
    route <- ndex.helper.encodeParams(api$url, api$params, user=uuuid)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


####################################################
##   CRUD functions for user
####################################################

#' Create a user
#' 
#' Create a new user based on a JSON object specifying username, password, and emailAddress. 
#' Username and emailAddress must be unique in the database.
#' If email verification is turned on on the server, this call returns code 220 (Accepted), the location field in the header has the URL to check the status of the newly created user account.
#' If email verification is turned on off on the server, this function returns 201 (Created). The URL for getting the newly created user is in the response body and the Location header.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param userName character; name of the new user
#' @param password character; password for the new user
#' @param emailAddress character (optional); email address (used for verification if enabled) 
#' @param isIndividual boolean (default: TRUE); True if this account is for an individual user. False means this account is for an organization or a project etc.
#' @param displayName character (optional); Display name of this account, only applied to non-individual accounts.
#' @param firstName character (optional); Account owner's first name, only applies to individual accounts.
#' @param lastName character (optional); Account owner's last name, only appliies to individual accounts.
#' @param image character (optional); URL of the account owner's image.
#' @param website character (optional); URL of the account owner's web site
#' @param description character (optional); Short description of this user.
#' @param verbose logical (optional); whether to print out extended feedback 
#'
#' @return UUID of the newly created user if email verification is turned off, else an empty string ("")
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$create
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' userId = ndex.create.user(ndexcon, 'SomeUserName', 'SecretPassword', 'SomeUserName@ndex.org')
#' ## [1] "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' 
#' userId = ndex.create.user(ndexcon, 'ASpecialProject', 'SecretPassword', 'ASpecialProject@ndex.org', isIndividual=TRUE, displayName='Area51', firstName='John', lastName='Doe', website='www.gidf.com', description='Nothing to see here..')
#' }
#' @export
ndex.create.user <- function(ndexcon, userName, password, emailAddress, isIndividual=TRUE, displayName, firstName, lastName, image, website, description, verbose=FALSE) {
    if(missing(userName)) stop('ndex.create.user: User name is required!')
    if(missing(password)) stop('ndex.create.user: Password is required!')
    if(missing(emailAddress) && verbose) warning('ndex.create.user: Email address is required, if server has email verification enabled!')    

    data = list(userName=userName, password=password, isIndividual=isIndividual)
    
    if(!missing(emailAddress) && !is.null(emailAddress)) data$emailAddress = emailAddress
    if(!missing(displayName) && !is.null(displayName)) data$displayName = displayName
    if(!missing(firstName) && !is.null(firstName)) data$firstName = firstName
    if(!missing(lastName) && !is.null(lastName)) data$lastName = lastName
    if(!missing(image) && !is.null(image)) data$image = image
    if(!missing(website) && !is.null(website)) data$website = website
    if(!missing(description) && !is.null(description)) data$description = description
    
    api = ndex.helper.getApi(ndexcon, 'user$create')
    route <- ndex.helper.encodeParams(api$url, api$params)
    data <- jsonlite::toJSON(data, auto_unbox = TRUE)
    
    response = ndex_rest_POST(ndexcon, route, data, raw=T)
    return(response)
}


#' Delete User
#' 
#' Deletes the authenticated user, removing any other objects in the database that depend on the user
#' 
#' @param ndexcon object of class NDEXConnection
#' @param uuid unique ID of the user
#' 
#' @return NULL if successfull, else an error is thrown
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$delete
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect('some.ndex.account','password')
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' ndexr.delete.user(ndexcon,user$externalId)
#' }
#' @export
ndex.delete.user <- function(ndexcon, uuid) {
    api = ndex.helper.getApi(ndexcon, 'user$delete')
    route <- ndex.helper.encodeParams(api$url, api$params, user=uuid)
    response = ndex_rest_DELETE(ndexcon, route, raw=T)
    return(NULL)
}


#' Update User
#' 
#' Updates the authenticated user based on the data. Errors, if the user for ndexcon and uuid are different.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param uuid character; unique ID of the user
#' @param emailAddress character (optional); email address (used for verification if enabled) 
#' @param isIndividual boolean (default:True); True if this account is for an individual user. False means this account is for an organization or a project etc.
#' @param displayName character (optional); Display name of this account, only applied to non-individual accounts.
#' @param firstName character (optional); Account owner's first name, only applies to individual accounts.
#' @param lastName character (optional); Account owner's last name, only appliies to individual accounts.
#' @param image character (optional); URL of the account owner's image.
#' @param website character (optional); URL of the account owner's web site
#' @param description character (optional); Short description of this user.
#' @param verbose logical (optional); whether to print out extended feedback  
#'
#' @return Empty string ("") on success, else error
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$update
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' ndex.update.user(ndexcon, user$externalId, firstName = 'Homer Jay', lastName = 'Simpson')
#' }
#' @export
ndex.update.user <- function(ndexcon, uuid, emailAddress, isIndividual, displayName, firstName, lastName, image, website, description, verbose=FALSE) {
    data = ndex.find.user.byId(ndexcon, uuid)
    
    data$password = NULL
    if(!missing(emailAddress) && !is.null(emailAddress)) data$emailAddress = emailAddress
    if(!missing(isIndividual) && !is.null(isIndividual)) data$isIndividual = isIndividual
    if(!missing(displayName) && !is.null(displayName)) data$displayName = displayName
    if(!missing(firstName) && !is.null(firstName)) data$firstName = firstName
    if(!missing(lastName) && !is.null(lastName)) data$lastName = lastName
    if(!missing(image) && !is.null(image)) data$image = image
    if(!missing(website) && !is.null(website)) data$website = website
    if(!missing(description) && !is.null(description)) data$description = description
    
    api = ndex.helper.getApi(ndexcon, 'user$update')
    route <- ndex.helper.encodeParams(api$url, api$params, user=uuid)
    data <- jsonlite::toJSON(data, auto_unbox = TRUE, null='null')
    
    response = ndex_rest_PUT(ndexcon, route, data, raw=T)
    return(response)
}


#' Verify a User
#' 
#' Verify the given user with UUID and verification code, which is set by email
#' 
#' @param ndexcon object of class NDEXConnection
#' @param uuid character; unique ID of the user
#' @param code character; Verification code sent by email
#' 
#' @return string "User account XXX has been activated." when this user's account is successfully activated.
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$verify
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' ndex.verify.user(ndexcon, user$externalId, 'Osqy11mRZ9')
#' ## [1] "User account XXX has been activated."
#' }
#' @export
ndex.verify.user <- function(ndexcon, uuid, code) {
    api = ndex.helper.getApi(ndexcon, 'user$verify')
    route <- ndex.helper.encodeParams(api$url, api$params, user=uuid, code=code)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


####################################################
##   User password
####################################################

#' Change Password
#' 
#' Changes the authenticated user's password to the new password
#' 
#' @param ndexcon object of class NDEXConnection
#' @param uuid character; unique ID of the user
#' @param password character; New password
#' 
#' @return Empty string on success, else error
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$password$change
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples
#' \dontrun{
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' ndex.user.change.password(ndexcon, user$externalId, 'SuperSaveNewPassword')
#' }
#' @export
ndex.user.change.password <- function(ndexcon, uuid, password) {
    api = ndex.helper.getApi(ndexcon, 'user$password$change')
    route <- ndex.helper.encodeParams(api$url, api$params, user=uuid)
    
    response = ndex_rest_PUT(ndexcon, route, data=password, raw=T)
    return(response)
}


#' Email New Password
#' 
#' Causes a new password to be generated for the given user account and then emailed to the user's emailAddress
#' 
#' @param ndexcon object of class NDEXConnection
#' @param uuid character; unique ID of the user
#' 
#' @return Empty string on success, else error
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$password$mail
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' ndex.user.mail.password(ndexcon, user$externalId)
#' }
#' @export
ndex.user.mail.password <- function(ndexcon, uuid) {
    api = ndex.helper.getApi(ndexcon, 'user$password$mail')
    route <- ndex.helper.encodeParams(api$url, api$params, user=uuid)
    
    response = ndex_rest_PUT(ndexcon, route, data=NULL, raw=T)
    return(response)
}


#' Forgot Password
#' 
#' Causes a new password to be generated for the given user account and then emailed to the user's emailAddress
#' 
#' @param ndexcon object of class NDEXConnection
#' @param uuid character; unique ID of the user
#' 
#' @return Empty string on success, else error
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$password$mail
#' Wrapper for ndex.user.mail.password()
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' ndex.user.forgot.password(ndexcon, user$externalId)
#' }
#' @export
ndex.user.forgot.password <- function(ndexcon, uuid) {
    return(ndex.user.mail.password(ndexcon, uuid))
}


####################################################
##   An User's Membership in Groups
####################################################

#' Get User's Membership in Group
#' 
#' Returns the permission that the user specified in the URL has on the given group. Returns an empty object if the authenticated user is not a member of this group.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param userId character; unique ID (UUID) of the user
#' @param groupId character; unique ID (UUID) of the group
#' 
#' @return List of permissions of that user or empty object
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$group$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' ## get user by name to get UUID
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' groupId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' 
#' ## get users's permission in the group
#' userPermissions = ndex.user.show.group(ndexcon, user$externalId, groupId)
#' userPermissions
#' ## $`aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee`
#' ## [1] "MEMBER"
#' }
#' @export
ndex.user.show.group <- function(ndexcon, userId, groupId) {
    api = ndex.helper.getApi(ndexcon, 'user$group$get')
    route <- ndex.helper.encodeParams(api$url, api$params, user=userId, group=groupId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


#' Get User's Group Memberships
#' 
#' Query finds groups for which the current user has the specified membership type. If the "type' parameter is omitted, all membership types will be returned. Returns a map which maps a group UUID to the membership type the authenticated user has.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param userId character; unique ID (UUID) of the user
#' @param type character (optional); constrains the type of the returned membership. If not set (or NULL), all permission types will be returned. 
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return List of permissions of that user or empty object
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$group$list
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' ## get user by name to get UUID
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' 
#' ## get all group memberships of the user to get the group UUID 
#' groupPermissions = ndex.user.list.groups(ndexcon, user$externalId)
#' ## $`aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee`
#' ## [1] "MEMBER"
#' 
#' groupIds = names(permissions)
#' ## [1] "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' }
#' @export 
ndex.user.list.groups <- function(ndexcon, userId, type=NULL, start=NULL, size=NULL) {
    api = ndex.helper.getApi(ndexcon, 'user$group$list')
    route <- ndex.helper.encodeParams(api$url, api$params, user=userId, type=type, start=start, size=size)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


####################################################
##   An User's Network Permissions
####################################################

#' Get User's Permission for Network
#' 
#' Get the type(s) of permission assigned to the authenticated user for the specified network. Returns a map which maps a network UUID to the highest permission assigned to the authenticated user.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param userId character; unique ID (UUID) of the user
#' @param networkId character; unique ID (UUID) of the group
#' @param directonly logical (default: FALSE); If directonly is set to true, permissions granted through groups are not included in the result
#' 
#' @return List of permissions of that user ("READ"|"WRITE"|"ADMIN") or empty object
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$permission$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' ## get user by name to get UUID
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' networkId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' 
#' ## get users's permission to a network
#' networkPermissions = ndex.user.show.permission(ndexcon, user$externalId, networkId, directonly=TRUE)
#' ## $`aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee`
#' ## [1] "ADMIN"
#' }
#' @export
ndex.user.show.permission <- function(ndexcon, userId, networkId, directonly=FALSE) {
    api = ndex.helper.getApi(ndexcon, 'user$permission$get')
    route <- ndex.helper.encodeParams(api$url, api$params, user=userId, network=networkId, directonly=directonly)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


#' Get User's Network Permissions
#' 
#' This function returns networks for which the authenticated user is assigned the specified permission. Userid is the UUID of the authenticated user. Returns a JSON map in which the keys are network UUIDs and values are the highest permission assigned to the authenticated user.#' 
#' 
#' @param ndexcon object of class NDEXConnection
#' @param userId character; unique ID (UUID) of the user
#' @param type character (optional)("READ"|"WRITE"|"ADMIN"); constrains the type of the returned permission. If not set (or NULL), all permission types will be returned.
#' @param directonly logical (default: FALSE); If directonly is set to true, permissions granted through groups are not included in the result
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return List of highest permissions of that user or empty object
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$permission$list
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' ## get user by name to get UUID
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' 
#' ## get all network permissions of the user
#' networkPermissions = ndex.user.list.permissions(ndexcon, user$externalId)
#' ## $`aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee`
#' ## [1] "ADMIN"
#' 
#' networkIds = names(permissions)
#' ## [1] "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
#' 
#' ## get all networks for which the user has Admin permissions
#' networkPermissions = ndex.user.list.permissions(ndexcon, user$externalId, type='ADMIN')
#' ## $`aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee`
#' ## [1] "ADMIN"
#' 
#' ## get all networks for which the user has direct access
#' networkPermissions = ndex.user.list.permissions(ndexcon, user$externalId, directonly=TRUE)
#' ## $`aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee`
#' ## [1] "ADMIN"
#' }
#' @export 
ndex.user.list.permissions <- function(ndexcon, userId, type=NULL, directonly=FALSE, start=NULL, size=NULL) {
    api = ndex.helper.getApi(ndexcon, 'user$permission$list')
    route <- ndex.helper.encodeParams(api$url, api$params, user=userId, permission=type, directonly=directonly, start=start, size=size)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


####################################################
##   Public user page
####################################################

#' Get User's Showcase Networks
#' 
#' This is a convenience function to support "user pages" in NDEx applications. 
#' This function returns a list of network summary objects that the user who is specified by userid chose to display in his or her home page. 
#' For authenticated users, this function returns the networks that the authenticated user can read, for anonymous users, this function returns only public networks.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param userId character; unique ID (UUID) of the user
#' 
#' @return data.frame of networks (name, description, externalId, uri, etc.) in the showcase of the specified user
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$showcase
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' ## get user by name to get UUID
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' 
#' showcase = ndex.user.get.showcase(con, user$externalId)
#' names(showcase)
#' ## [1] "ownerUUID"        "isReadOnly"       "subnetworkIds"    "errorMessage"     "isValid"         
#' ## [6] "warnings"         "isShowcase"       "visibility"       "edgeCount"        "nodeCount"       
#' ##[11] "uri"              "version"          "owner"            "name"             "properties"      
#' ##[16] "description"      "externalId"       "isDeleted"        "modificationTime" "creationTime"
#' }   
#' @export 
ndex.user.get.showcase <- function(ndexcon, userId) {
    api = ndex.helper.getApi(ndexcon, 'user$showcase')
    route <- ndex.helper.encodeParams(api$url, api$params, user=userId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


#' Get User's Account Page Networks
#' 
#' This is a convenience function designed to support "My Account" pages in NDEx applications. 
#' It returns a list of NetworkSummary objects to display.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param userId character; unique ID (UUID) of the user
#' 
#' @return data.frame of networks (name, description, externalId, uri, etc.) on the account page of the specified user
#' 
#' @section REST query:
#' GET: ndex.api.config$api$user$networksummary
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' \dontrun{
#' ## get user by name to get UUID
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' 
#' networkSummary = ndex.user.get.networksummary(con, user$externalId)
#' names(networkSummary)
#' ## [1] "ownerUUID"        "isReadOnly"       "subnetworkIds"    "errorMessage"     "isValid"         
#' ## [6] "warnings"         "isShowcase"       "visibility"       "edgeCount"        "nodeCount"       
#' ##[11] "uri"              "version"          "owner"            "name"             "properties"      
#' ##[16] "description"      "externalId"       "isDeleted"        "modificationTime" "creationTime"
#' }  
#' @export 
ndex.user.get.networksummary <- function(ndexcon, userId) {
    api = ndex.helper.getApi(ndexcon, 'user$networksummary')
    route <- ndex.helper.encodeParams(api$url, api$params, user=userId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}