################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 23.03.2017 by Auer
## 	
## Description:
##
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
#' @return list of properties describing the user (externalId, emailAddress, website, etc.). Throws error (404) if user isn't found!
#' @section REST query:
#' GET: ndex.api.config$api$user$get$byName
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @examples 
#' user = ndex.find.user.byName(ndexcon, 'someAccountName')
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
#' @return list of properties describing the user (externalId, emailAddress, website, etc.). Throws error (404) if user isn't found!
#' @section REST query:
#' GET: ndex.api.config$api$user$get$byId
#' @note Compatible to NDEx server version 1.3 and 2.0
#' @examples 
#' user = ndex.find.user.byName(ndexcon, 'someAccountName')
#' user = ndex.find.user.byId(ndexcon, user$externalId)
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
#' @param isIndividual boolean (default:True); True if this account is for an individual user. False means this account is for an organization or a project etc.
#' @param displayName character (optional); Display name of this account, only applied to non-individual accounts.
#' @param firstName character (optional); Account owner’s first name, only applies to individual accounts.
#' @param lastName character (optional); Account owner’s last name, only appliies to individual accounts.
#' @param image character (optional); URL of the account owner’s image.
#' @param website character (optional); URL of the account owner’s web site
#' @param description character (optional); Short description of this user.
#' @param verbose logical (optional); whether to print out extended feedback 
#'
#' @return UUID of the newly created user if email verification is turned off, else an empty string ("")
#' @examples 
#' 
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
#' @examples 
#' ndexcon = ndex.connect('some.ndex.account','password')
#' user = ndex.find.user.byName(ndexcon, 'some.ndex.account')
#' ndexr.delete.user(ndexcon,user$externalId)
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
#' @param password character; password for the new user
#' @param emailAddress character (optional); email address (used for verification if enabled) 
#' @param isIndividual boolean (default:True); True if this account is for an individual user. False means this account is for an organization or a project etc.
#' @param displayName character (optional); Display name of this account, only applied to non-individual accounts.
#' @param firstName character (optional); Account owner’s first name, only applies to individual accounts.
#' @param lastName character (optional); Account owner’s last name, only appliies to individual accounts.
#' @param image character (optional); URL of the account owner’s image.
#' @param website character (optional); URL of the account owner’s web site
#' @param description character (optional); Short description of this user.
#' @param verbose 
#'
#' @return Empty string ("") on success, else error
#' @examples 
#' ndex.update.user(ndexcon, user$externalId, firstName = 'Homer Jay', lastName = 'Simpson')
#' @export
ndex.update.user <- function(ndexcon, uuid, password, emailAddress, isIndividual, displayName, firstName, lastName, image, website, description, verbose=FALSE) {
	data = ndex.find.user.byId(ndexcon, uuid)
	
	if(!missing(password) && !is.null(password)) data$password = password
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
#' @return string “User account XXX has been activated.” when this user’s account is successfully activated.
#' @examples 
#' 
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

#' Name
#' 
#' Description
#' 
#' @param param
#' @return result
#' @examples 
#' 
#' 
ndex.user.change.password <- function(uuid, password) {
	
	return(NULL)
}


#' Name
#' 
#' Description
#' 
#' @param param
#' @return result
#' @examples 
#' 
#' 
ndex.user.mail.password <- function(uuid) {
	
	return(NULL)
}


#' Name
#' 
#' Description
#' 
#' @param param
#' @return result
#' @examples 
#' 
#' 
ndex.user.forgot.password <- function(uuid) {
	
	return(NULL)
}





#' Name
#' 
#' Description
#' 
#' @param param
#' @return result
#' @examples 
#' 
#' 
ndex.user.show.group <- function(uuid) {
	
	return(NULL)
}


#' Name
#' 
#' Description
#' 
#' @param param
#' @return result
#' @examples 
#' 
#' 
ndex.user.list.groups <- function(uuid) {
	
	return(NULL)
}


#' Name
#' 
#' Description
#' 
#' @param param
#' @return result
#' @examples 
#' 
#' 
ndex.user.show.permission <- function(uuid) {
	
	return(NULL)
}


#' Name
#' 
#' Description
#' 
#' @param param
#' @return result
#' @examples 
#' 
#' 
ndex.user.list.permissions <- function(uuid) {
	
	return(NULL)
}


#' Name
#' 
#' Description
#' 
#' @param param
#' @return result
#' @examples 
#' 
#' 
ndex.user.get.showcase <- function(uuid) {
	
	return(NULL)
}


#' Name
#' 
#' Description
#' 
#' @param param
#' @return result
#' @examples 
#' 
#' 
ndex.user.get.networksummary <- function(uuid) {
	
	return(NULL)
}