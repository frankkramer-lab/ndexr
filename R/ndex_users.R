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

#' Search user in NDEx
#' 
#' Returns a SearchResult object which contains an array of User objects
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param searchString string by which to search
#' @param start integer (optional); specifies that the result is the nth page of the requested data. The default value is 0
#' @param size integer (optional); specifies the number of data items in each page. The default value is 100
#' 
#' @return Data frame with user information; NULL if no user are found.
#' 
#' @section REST query:
#' GET: ndex_config$api$search$user
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @note Search strings may be structured
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find a user
#' users = ndex_find_users(ndexcon,"ndextutorials") 
#' names(users)
#' ## [1] "properties"       "displayName"      "isIndividual"     "userName"         "password"        
#' ## [6] "isVerified"       "firstName"        "lastName"         "diskQuota"        "diskUsed"        
#' ##[11] "emailAddress"     "image"            "website"          "description"      "externalId"      
#' ##[16] "isDeleted"        "modificationTime" "creationTime"    
#' @export
ndex_find_users <- function(ndexcon, searchString="", start, size){
    
    if (missing(start)) start = NULL
    if (missing(size)) size = NULL
    
    ##Form JSON to post
    query = list(searchString=searchString)
    query <- jsonlite::toJSON(query, auto_unbox = TRUE)
    
    ##Form route
    api = ndex_helper_getApi(ndexcon, 'search$user')
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

#' Get User By Name
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param name name of the user
#' 
#' @return list of properties describing the user (externalId, emailAddress, website, etc.). Throws error (404) if user isn't found!
#' 
#' @section REST query:
#' GET: ndex_config$api$user$get$byName
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find user by name
#' user = ndex_find_user_byName(ndexcon, 'ndextutorials')
#' @export
ndex_find_user_byName <- function(ndexcon, name) {
    api = ndex_helper_getApi(ndexcon, 'user$get$byName')
    route <- ndex_helper_encodeParams(api$url, api$params, userName=name)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


#' Get User By UUID
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID (UUID) of the user
#' 
#' @return list of properties describing the user (externalId, emailAddress, website, etc.). Throws error (404) if user isn't found!
#' 
#' @section REST query:
#' GET: ndex_config$api$user$get$byId
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find user by name
#' user = ndex_find_user_byName(ndexcon, 'ndextutorials')
#' ## Find user by Id
#' user = ndex_find_user_byId(ndexcon, user$externalId)
#' @export
ndex_find_user_byId <- function(ndexcon, userId) {
    api = ndex_helper_getApi(ndexcon, 'user$get$byId')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId)
    
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
#' @param ndexcon object of class NDExConnection link{ndex_connect}
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
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' GET: ndex_config$api$user$create
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## Create a new user
#' # userId = ndex_create_user(ndexcon, 'SomeUserName', 'SecretPassword', 'SomeUserName@ndex.org')
#' ## [1] "uuuuuuuu-ssss-eeee-rrrr-123456789abc"
#' # userId = ndex_create_user(ndexcon, 'ASpecialProject', 'SecretPassword', 'ASpecialProject@ndex.org', isIndividual=TRUE, displayName='Area51', firstName='John', lastName='Doe', website='www.gidf.com', description='Nothing to see here..')
#' NULL
#' @export
ndex_create_user <- function(ndexcon, userName, password, emailAddress, isIndividual=TRUE, displayName, firstName, lastName, image, website, description, verbose=FALSE) {
    if(missing(userName)) stop('ndex_create_user: User name is required!')
    if(missing(password)) stop('ndex_create_user: Password is required!')
    if(missing(emailAddress) && verbose) warning('ndex_create_user: Email address is required, if server has email verification enabled!')    

    data = list(userName=userName, password=password, isIndividual=isIndividual)
    
    if(!missing(emailAddress) && !is.null(emailAddress)) data$emailAddress = emailAddress
    if(!missing(displayName) && !is.null(displayName)) data$displayName = displayName
    if(!missing(firstName) && !is.null(firstName)) data$firstName = firstName
    if(!missing(lastName) && !is.null(lastName)) data$lastName = lastName
    if(!missing(image) && !is.null(image)) data$image = image
    if(!missing(website) && !is.null(website)) data$website = website
    if(!missing(description) && !is.null(description)) data$description = description
    
    api = ndex_helper_getApi(ndexcon, 'user$create')
    route <- ndex_helper_encodeParams(api$url, api$params)
    data <- jsonlite::toJSON(data, auto_unbox = TRUE)
    
    response = ndex_rest_POST(ndexcon, route, data, raw=TRUE)
    return(response)
}


#' Delete User
#' 
#' Deletes the authenticated user, removing any other objects in the database that depend on the user
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID (UUID) of the user
#' 
#' @return NULL if successfull, else an error is thrown
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' GET: ndex_config$api$user$delete
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## Find user and get its id
#' # user = ndex_find_user_byName(ndexcon, 'SomeUserName')
#' # userId = user$externalId
#' ## Delete user
#' # ndexr.delete.user(ndexcon, userId)
#' NULL
#' @export
ndex_delete_user <- function(ndexcon, userId) {
    api = ndex_helper_getApi(ndexcon, 'user$delete')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId)
    response = ndex_rest_DELETE(ndexcon, route, raw=TRUE)
    return(NULL)
}


#' Update User
#' 
#' Updates the authenticated user based on the data. Errors, if the user for ndexcon and uuid are different.
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID of the user
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
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' GET: ndex_config$api$user$update
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## Find user and get its id
#' # user = ndex_find_user_byName(ndexcon, 'SomeUserName')
#' # userId = user$externalId
#' ## Update user
#' # ndex_update_user(ndexcon, userId, firstName = 'Homer Jay', lastName = 'Simpson')
#' # ndex_update_user(ndexcon, userId, displayName = 'Max Power', image='https://upload.wikimedia.org/wikipedia/en/0/02/Homer_Simpson_2006.png', description='One of the most influential characters in the history of television')
#' NULL
#' @export
ndex_update_user <- function(ndexcon, userId, emailAddress, isIndividual, displayName, firstName, lastName, image, website, description, verbose=FALSE) {
    data = ndex_find_user_byId(ndexcon, userId)
    
    data$password = NULL
    if(!missing(emailAddress) && !is.null(emailAddress)) data$emailAddress = emailAddress
    if(!missing(isIndividual) && !is.null(isIndividual)) data$isIndividual = isIndividual
    if(!missing(displayName) && !is.null(displayName)) data$displayName = displayName
    if(!missing(firstName) && !is.null(firstName)) data$firstName = firstName
    if(!missing(lastName) && !is.null(lastName)) data$lastName = lastName
    if(!missing(image) && !is.null(image)) data$image = image
    if(!missing(website) && !is.null(website)) data$website = website
    if(!missing(description) && !is.null(description)) data$description = description
    
    api = ndex_helper_getApi(ndexcon, 'user$update')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId)
    data <- jsonlite::toJSON(data, auto_unbox = TRUE, null='null')
    
    response = ndex_rest_PUT(ndexcon, route, data, raw=TRUE)
    return(response)
}


#' Verify a User
#' 
#' Verify the given user with UUID and verification code, which is set by email
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID of the user
#' @param code character; Verification code sent by email
#' 
#' @return string "User account XXX has been activated." when this user's account is successfully activated.
#' 
#' @section REST query:
#' GET: ndex_config$api$user$verify
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' # ndexcon = ndex_connect()
#' ## Find user and get its id
#' # user = ndex_find_user_byName(ndexcon, 'SomeUserName')
#' # userId = user$externalId
#' ## Verify user with verification code
#' # ndex_verify_user(ndexcon, userId, 'Osqy11mRZ9')
#' ## [1] "User account XXX has been activated."
#' NULL
#' @export
ndex_verify_user <- function(ndexcon, userId, code) {
    api = ndex_helper_getApi(ndexcon, 'user$verify')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId, code=code)
    
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
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID of the user
#' @param password character; New password
#' 
#' @return Empty string on success, else error
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' GET: ndex_config$api$user$password$change
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## Find user and get its id
#' # user = ndex_find_user_byName(ndexcon, 'SomeUserName')
#' # userId = user$externalId
#' ## Change user password
#' # ndex_user_change_password(ndexcon, userId, 'SuperSaveNewPassword')
#' NULL
#' @export
ndex_user_change_password <- function(ndexcon, userId, password) {
    api = ndex_helper_getApi(ndexcon, 'user$password$change')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId)
    
    response = ndex_rest_PUT(ndexcon, route, data=password, raw=TRUE)
    return(response)
}


#' Email New Password
#' 
#' Causes a new password to be generated for the given user account and then emailed to the user's emailAddress
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID of the user
#' 
#' @return Empty string on success, else error
#' 
#' @section REST query:
#' GET: ndex_config$api$user$password$mail
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' # ndexcon = ndex_connect()
#' ## Find user and get its id
#' # user = ndex_find_user_byName(ndexcon, 'SomeUserName')
#' # userId = user$externalId
#' ## Request new password via email
#' # ndex_user_mail_password(ndexcon, userId)
#' NULL
#' @export
ndex_user_mail_password <- function(ndexcon, userId) {
    api = ndex_helper_getApi(ndexcon, 'user$password$mail')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId)
    
    response = ndex_rest_PUT(ndexcon, route, data=NULL, raw=TRUE)
    return(response)
}


#' Forgot Password
#' 
#' Causes a new password to be generated for the given user account and then emailed to the user's emailAddress
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID of the user
#' 
#' @return Empty string on success, else error
#' 
#' @section REST query:
#' GET: ndex_config$api$user$password$mail
#' Wrapper for ndex_user_mail_password()
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' # ndexcon = ndex_connect()
#' ## Find user and get its id
#' # user = ndex_find_user_byName(ndexcon, 'SomeUserName')
#' # userId = user$externalId
#' ## Request new password via email
#' # ndex_user_forgot_password(ndexcon, userId)
#' NULL
#' @export
ndex_user_forgot_password <- function(ndexcon, userId) {
    return(ndex_user_mail_password(ndexcon, userId))
}


####################################################
##   An User's Membership in Groups
####################################################

#' Get User's Membership in Group
#' 
#' Returns the permission that the user specified in the URL has on the given group. Returns an empty object if the authenticated user is not a member of this group.
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID (UUID) of the user
#' @param groupId character; unique ID (UUID) of the group
#' 
#' @return List of permissions of that user or empty object
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' GET: ndex_config$api$user$group$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## Find user and get its id
#' # user = ndex_find_user_byName(ndexcon, 'SomeUserName')
#' # userId = user$externalId
#' ## Find the user's groups and get one group id
#' # groups = ndex_user_list_groups(ndexcon, userId)
#' # groupId = groups[1,"externalId"]
#' ## get users's permission in the group
#' # userPermissions = ndex_user_show_group(ndexcon, userId, groupId)
#' ## $`uuuuuuuu-ssss-eeee-rrrr-123456789abc`
#' ## [1] "MEMBER"
#' NULL
#' @export
ndex_user_show_group <- function(ndexcon, userId, groupId) {
    api = ndex_helper_getApi(ndexcon, 'user$group$get')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId, group=groupId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


#' Get User's Group Memberships
#' 
#' Query finds groups for which the current user has the specified membership type. If the "type' parameter is omitted, all membership types will be returned. Returns a map which maps a group UUID to the membership type the authenticated user has.
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID (UUID) of the user
#' @param type character (optional)("MEMBER"|"GROUPADMIN"); constrains the type of the returned membership. If not set (or NULL), all permission types will be returned. 
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return List of permissions of that user or empty object
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' GET: ndex_config$api$user$group$list
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
#' ## $`ggggggg-rrrr-oooo-uuuu-pppppp111111`
#' ## [1] "MEMBER"
#' ##
#' ## $`ggggggg-rrrr-oooo-uuuu-pppppp222222`
#' ## [1] "GROUPADMIN"
#' # groupIds = names(groups)
#' ## [1] "ggggggg-rrrr-oooo-uuuu-pppppp111111" "ggggggg-rrrr-oooo-uuuu-pppppp222222"
#' NULL
#' @export 
ndex_user_list_groups <- function(ndexcon, userId, type=NULL, start=NULL, size=NULL) {
    api = ndex_helper_getApi(ndexcon, 'user$group$list')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId, type=type, start=start, size=size)
    
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
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID (UUID) of the user
#' @param networkId character; unique ID (UUID) of the group
#' @param directonly logical (default: FALSE); If directonly is set to true, permissions granted through groups are not included in the result
#' 
#' @return List of permissions of that user ("READ"|"WRITE"|"ADMIN") or empty object
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' GET: ndex_config$api$user$permission$get
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## get user by name to get UUID
#' # user = ndex_find_user_byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## Find one of your networks and get its UUID
#' # networks = ndex_find_networks(ndexcon, accountName='MyAccountName')
#' # networkId = networks[1,"externalId"]
#' ## get users's permission to a network
#' # networkPermissions = ndex_user_show_permission(ndexcon, userId, networkId, directonly=TRUE)
#' ## $`nnneeett-wwww-oooo-rrrr-kkkkkkk11111`
#' ## [1] "ADMIN"
#' NULL
#' @seealso \link{ndex_network_get_permission}
#' @export
ndex_user_show_permission <- function(ndexcon, userId, networkId, directonly=FALSE) {
    api = ndex_helper_getApi(ndexcon, 'user$permission$get')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId, network=networkId, directonly=directonly)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


#' Get User's Network Permissions
#' 
#' This function returns networks for which the authenticated user is assigned the specified permission. Userid is the UUID of the authenticated user. Returns a JSON map in which the keys are network UUIDs and values are the highest permission assigned to the authenticated user.#' 
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID (UUID) of the user
#' @param type character (optional)("READ"|"WRITE"|"ADMIN"); constrains the type of the returned permission. If not set (or NULL), all permission types will be returned.
#' @param directonly logical (default: FALSE); If directonly is set to true, permissions granted through groups are not included in the result
#' @param start integer (optional); specifies that the result is the nth page of the requested data.
#' @param size integer (optional); specifies the number of data items in each page.
#' 
#' @return List of highest permissions of that user or empty object
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' GET: ndex_config$api$user$permission$list
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## get user by name to get UUID
#' # user = ndex_find_user_byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## get all network permissions of the user
#' # networkPermissions = ndex_user_list_permissions(ndexcon, userId)
#' ## $`nnneeett-wwww-oooo-rrrr-kkkkkkk11111`
#' ## [1] "ADMIN"
#' ## $`nnneeett-wwww-oooo-rrrr-kkkkkkk22222`
#' ## [1] "WRITE"
#' ## $`nnneeett-wwww-oooo-rrrr-kkkkkkk33333`
#' ## [1] "READ"
#' # networkIds = names(networkPermissions)
#' ## [1] "nnneeett-wwww-oooo-rrrr-kkkkkkk11111" "nnneeett-wwww-oooo-rrrr-kkkkkkk22222"
#' ## [3] "nnneeett-wwww-oooo-rrrr-kkkkkkk33333" 
#' ## get all networks for which the user has Admin permissions
#' # networkPermissions = ndex_user_list_permissions(ndexcon, userId, type='ADMIN')
#' ## $`nnneeett-wwww-oooo-rrrr-kkkkkkk11111`
#' ## [1] "ADMIN"
#' ## get all networks for which the user has direct access
#' # networkPermissions = ndex_user_list_permissions(ndexcon, user$externalId, directonly=TRUE)
#' ## $`nnneeett-wwww-oooo-rrrr-kkkkkkk11111`
#' ## [1] "ADMIN"
#' NULL
#' @export 
ndex_user_list_permissions <- function(ndexcon, userId, type=NULL, directonly=FALSE, start=NULL, size=NULL) {
    api = ndex_helper_getApi(ndexcon, 'user$permission$list')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId, permission=type, directonly=directonly, start=start, size=size)
    
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
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID (UUID) of the user
#' 
#' @return data.frame of networks (name, description, externalId, uri, etc.) in the showcase of the specified user
#' 
#' @section REST query:
#' GET: ndex_config$api$user$showcase
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## get user by name to get UUID
#' user = ndex_find_user_byName(ndexcon, 'ndextutorials')
#' userId = user$externalId
#' ## get all network permissions of the user
#' showcase = ndex_user_get_showcase(ndexcon, userId)
#' names(showcase)
#' ## [1] "ownerUUID"        "isReadOnly"       "subnetworkIds"    "errorMessage"     "isValid"         
#' ## [6] "warnings"         "isShowcase"       "visibility"       "edgeCount"        "nodeCount"       
#' ##[11] "uri"              "version"          "owner"            "name"             "properties"      
#' ##[16] "description"      "externalId"       "isDeleted"        "modificationTime" "creationTime"
#' @export 
ndex_user_get_showcase <- function(ndexcon, userId) {
    api = ndex_helper_getApi(ndexcon, 'user$showcase')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}


#' Get User's Account Page Networks
#' 
#' This is a convenience function designed to support "My Account" pages in NDEx applications. 
#' It returns a list of NetworkSummary objects to display.
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param userId character; unique ID (UUID) of the user
#' 
#' @return data.frame of networks (name, description, externalId, uri, etc.) on the account page of the specified user
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' GET: ndex_config$api$user$networksummary
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## get user by name to get UUID
#' # user = ndex_find_user_byName(ndexcon, 'MyAccountName')
#' # userId = user$externalId
#' ## get all network permissions of the user
#' # networkSummary = ndex_user_get_networksummary(con, user$externalId)
#' # names(networkSummary)
#' ## [1] "ownerUUID"        "isReadOnly"       "subnetworkIds"    "errorMessage"     "isValid"         
#' ## [6] "warnings"         "isShowcase"       "visibility"       "edgeCount"        "nodeCount"       
#' ##[11] "uri"              "version"          "owner"            "name"             "properties"      
#' ##[16] "description"      "externalId"       "isDeleted"        "modificationTime" "creationTime"
#' NULL
#' @export 
ndex_user_get_networksummary <- function(ndexcon, userId) {
    api = ndex_helper_getApi(ndexcon, 'user$networksummary')
    route <- ndex_helper_encodeParams(api$url, api$params, user=userId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}