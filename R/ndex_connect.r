################################################################################
## Authors:
##   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##   Dexter Pratt [depratt@ucsd.edu]
##   Frank Kramer [frank.kramer@med.uni-goettingen.de]
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 1 June 2014 by Ishkin
## 	
## Description:
##   Base functions to perform HTTP transactions to an NDEX server via the NDEx REST API
##   Updated to NDEX v1.0 API 1 November 2014
##   Updated to use NDEXConnection object to store connection informations 15 September 2016
##   Updated to NDEX v2.0 API December 2016
################################################################################


#' Connect to NDEx REST API
#' 
#' This function creates an NDEXConnection which stores options and authentication details. It is a parameter required for most of the other ndexr functions.
#' If username and password are missing an anonymous connection is created, which already offers most of the retrieval functionality.
#' 
#' @param username character (optional); username
#' @param password character (optional); password
#' @param host character (optional); URL of NDEx REST server; Set in apiConfig$defaults$connection$host. By default the host is "http://www.ndexbio.org"
#' @param apiConfig config object (neste list, optional); Configuration of NDEx REST server; Set in ndex.api.config (or ndex.api.yml): It contains specifications for NDEx server api version 1.3 and 2.0. The default api is specified by 'defaultVersion'
#' @param verbose logical (optional); whether to print out extended feedback 
#' @return returns object of class NDEXConnection which stores options, authentication and api configuration if successfull, NULL otherwise
#' @export
#' @examples
#' \dontrun{ndexcon = ndex.connect()   ## log in anonymously
#' ndexcon = ndex.connect(verbose=T)   ## same as above
#' ndexcon = ndex.connect('user','password')   ## log in with credentials
#' ndexcon = ndex.connect(host='localhost:8765')   ## running some NDEx server locally
#' ndexcon = ndex.connect(apiConfig=ndex.api.config$Version2.0)   ## manually change the api and connection configuration}
#' @seealso  \code{\link{ndex.api.config}}, \code{\link{updateConfigFromYaml}}
ndex.connect <- function(username, password, host = "apiConfig$defaults$connection$host", apiPath = 'apiConfig$defaults$connection$api', apiConfig=ndex.api.config, verbose = T){

  ##Check parameters and set defaults by config
  credentials = TRUE
  if(missing(username) || missing(password)){
    message("ndex.connect: Connecting anonymously - username or password not supplied")
    credentials = FALSE
  }
  
  if(missing(apiConfig)){
    defaultVersion = apiConfig$defaultVersion
    apiConfig=apiConfig[[defaultVersion]]
    if(verbose) message(paste("ndex.connect: apiConfig not specified, using default version ", apiConfig$version, " [", defaultVersion, "]" ))
  } else {
    if(verbose) message(paste("ndex.connect: Using apiConfig for version ", apiConfig$version))
  }
  
  if(missing(apiPath)){
	  apiPath = apiConfig$defaults$connection$api
	  if(verbose) message(paste("ndex.connect: apiPath not specified, using default: [", apiPath, "]" ))
  }
  
  if(missing(host)){
	  host = paste0(apiConfig$defaults$connection$host, apiPath)
	  if(verbose) message(paste("ndex.connect: Host not specified, using default: [", host, "]" ))
  } else {
	  host = paste0(host, apiPath)
	  if(verbose) message(paste("ndex.connect: Using host: [", host, "]" ))
  }
  
  ##Setup server connection, with and without credentials
  ##Check if server is available
  auth_param = NULL
  url <- paste0(host, apiConfig$api$serverStatus$url)
  log_txt = paste0("ndex.connect: Tried to check the server status of [", host, "]")

  ##Try to connect to the server; throws error if something went wrong
  response = ndex.helper.httpResponseHandler(httr::GET(url=url, config=auth_param), 
                                             log_txt, 
                                             verbose)
  if(verbose) message(paste("ndex.connect: Server response: ", response))
  
  ##Checkt the provided credentials
  if (credentials){
	  url <- paste0(host, apiConfig$api$user$authenticate$url)
	  auth_param = httr::authenticate(username, password)
    log_txt = paste0("ndex.connect: Tried to autheticate user: ", username)
    response = ndex.helper.httpResponseHandler(httr::GET(url=url, config=auth_param), 
                                               log_txt, 
                                               verbose)
    if(verbose) message(paste("ndex.connect: Server response: ", response))	
  }
						 
  
  ##Create ndexcon object
  ndexcon = list(anonymous=TRUE, host=host, apiConfig=apiConfig, verbose=verbose)
  if(credentials) {
    #ndexcon = list(anon=FALSE, credentials=credentials, current.user= auth_response$accountName, curl.opts=ndex.opts, host=host, apiversion=apiversion, verbose=verbose)
    ndexcon$anonymous = F
    ndexcon$username = username
    ndexcon$password = password
  }
  class(ndexcon) = c("NDExConnection",class(ndexcon))
  return(ndexcon)
}


####################################################
###
###   Low-level REST-querying functions
###
####################################################

#' Generic GET query to API. 
#' 
#' This functions is internal.
#' 
#' @param ndexcon object of class NDEXConnection \code{\link{ndex.connect}}
#' @param route Character (route to specific REST query)
#' @param raw Specifies if server response should be returned in raw, or if jsonlite::fromJSON is called first. Defaults to FALSE.
#' @return JSON response from REST server, NULL if no valid JSON was received. if parameter raw is TRUE, the raw response is returned without a call to jsonlite::fromJSON.
#' @details Simply execute HTTP GET on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' @seealso \code{\link{ndex_rest_GET}},  \code{\link{ndex_rest_POST}},  \code{\link{ndex_rest_PUT}} and \code{\link{ndex_rest_DELETE}}
#' @examples
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' ndex_rest_GET(ndexcon, "/networks/api")
#' }
ndex_rest_GET <- function(ndexcon, route, raw = FALSE){
  url <- paste0(ndexcon$host, route)
  auth <- NULL
  if(! ndexcon$anonymous) auth = httr::authenticate(ndexcon$username, ndexcon$password)
  
  try(response <- httr::GET(url, auth))
  ndex.helper.httpResponseHandler(response, paste("GET: [", url, "]"), ndexcon$verbose)
  content <- content(response, as='text')
  
  if(ndexcon$verbose) message('Response:', substring(content, 1, 300), '...', sep = '\n')
  if(raw) return(content)
  if(jsonlite::validate(content)) {
    return(jsonlite::fromJSON(content))
  } else {
    return(NULL)
  }
}


#' Generic POST query to API
#' 
#' @param ndexcon object of class NDEXConnection \code{\link{ndex.connect}}
#' @param route Character (route to specific REST query)
#' @param data Whatever data to be supplied with query. Should be valid JSON
#' @param multipart Whatever data to be supplied with query. Should be valid JSON
#' @param raw Specifies if server response should be returned in raw, or if jsonlite::fromJSON is called first. Defaults to FALSE.
#' @return JSON response from REST server, NULL if no valid JSON was received. if parameter raw is TRUE, the raw response is returned without a call to jsonlite::fromJSON.
#' @details Simply execute HTTP POST on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' Making sure the data is well-formed is also the job of calling function
#' @seealso \code{\link{ndex_rest_GET}},  \code{\link{ndex_rest_POST}},  \code{\link{ndex_rest_PUT}} and \code{\link{ndex_rest_DELETE}}
#' @examples
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' ndex_rest_POST(ndexcon, "/networks/api", data)
#' ndex_rest_POST(ndexcon, "/networks/api", data, raw=T)
#' ndex_rest_POST(ndexcon, "/networks/api", list(some=data, other=data2), multipart=T)
#' }
ndex_rest_POST <- function(ndexcon, route, data, multipart = FALSE, raw = FALSE){
  url <- paste0(ndexcon$host, route)
  auth <- NULL
  if(! ndexcon$anonymous) auth <- httr::authenticate(ndexcon$username, ndexcon$password)
  encode <- ifelse(multipart, 'multipart', 'json')
  contenttype <- content_type_json()
  if(multipart) contenttype <- content_type('multipart/form-data')
  
  try(response <- httr::POST(url, auth, contenttype, body = data, encode = encode))
  
  ndex.helper.httpResponseHandler(response, paste("POST: [", url, "]\ndata:\n",substring(data, 1, 300),'\n...'), ndexcon$verbose)
  content <- content(response, as='text')
  
  if(ndexcon$verbose) message('Response:', substring(content, 1, 300), '...', sep = '\n')
  if(raw) return(content)
  if(jsonlite::validate(content)) {
    return(jsonlite::fromJSON(content))
  } else {
    return(NULL)
  }
}


#' Generic PUT query to API
#' 
#' This functions is internal.
#' 
#' @param ndexcon object of class NDEXConnection \code{\link{ndex.connect}}
#' @param route Character (route to specific REST query)
#' @param data Whatever data to be supplied with query. Should be valid JSON
#' @param multipart Whatever data to be supplied with query. Should be valid JSON
#' @param raw Specifies if server response should be returned in raw, or if jsonlite::fromJSON is called first. Defaults to FALSE.
#' @return JSON response from REST server, NULL if no valid JSON was received. if parameter raw is TRUE, the raw response is returned without a call to jsonlite::fromJSON.
#' @details Simply execute HTTP PUT on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' Making sure the data is well-formed is also the job of calling function
#' @seealso \code{\link{ndex_rest_GET}},  \code{\link{ndex_rest_POST}},  \code{\link{ndex_rest_PUT}} and \code{\link{ndex_rest_DELETE}}
#' @examples
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' ndex_rest_PUT(ndexcon, "/networks/api", data)
#' ndex_rest_PUT(ndexcon, "/networks/api", data, raw=T)
#' ndex_rest_PUT(ndexcon, "/networks/api", list(some=data, other=data2), multipart=T)
#' }
ndex_rest_PUT <- function(ndexcon, route, data, multipart = FALSE, raw = FALSE){
	url <- paste0(ndexcon$host, route)
	auth <- NULL
	if(! ndexcon$anonymous) auth <- httr::authenticate(ndexcon$username, ndexcon$password)
	encode <- ifelse(multipart, 'multipart', 'json')
	contenttype <- content_type_json()
	if(multipart) contenttype <- content_type('multipart/form-data')
	
	try(response <- httr::PUT(url, auth, contenttype, body = data, encode = encode))
	
	ndex.helper.httpResponseHandler(response, paste("PUT: [", url, "]\ndata:\n",substring(data, 1, 300),'\n...'), ndexcon$verbose)
	content <- content(response, as='text')
	
	if(ndexcon$verbose) message('Response:', substring(content, 1, 300), '...', sep = '\n')
	if(raw) return(content)
	if(jsonlite::validate(content)) {
		return(jsonlite::fromJSON(content))
	} else {
		return(NULL)
	}
}


#' Generic GET query to API. 
#' 
#' This functions is internal.
#' 
#' @param ndexcon object of class NDEXConnection \code{\link{ndex.connect}}
#' @param route Character (route to specific REST query)
#' @param raw Specifies if server response should be returned in raw, or if jsonlite::fromJSON is called first. Defaults to FALSE.
#' @return JSON response from REST server, NULL if no valid JSON was received. if parameter raw is TRUE, the raw response is returned without a call to jsonlite::fromJSON.
#' @details Simply execute HTTP DELETE on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' @seealso \code{\link{ndex_rest_GET}},  \code{\link{ndex_rest_POST}},  \code{\link{ndex_rest_PUT}} and \code{\link{ndex_rest_DELETE}}
#' @examples
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' ndex_rest_DELETE(ndexcon, "/networks/api")
#' }
ndex_rest_DELETE <- function(ndexcon, route, raw = FALSE){
  url <- paste0(ndexcon$host, route)
  auth <- NULL
  if(! ndexcon$anonymous) auth = httr::authenticate(ndexcon$username, ndexcon$password)
  
  try(response <- httr::DELETE(url, auth, encode = 'json'))
  ndex.helper.httpResponseHandler(response, paste("DELETE: [", url, "]"), ndexcon$verbose)
  content <- content(response, as='text')
  
  if(ndexcon$verbose) message('Response:', substring(content, 1, 300), '...', sep = '\n')
  if(raw) return(content)
  if(jsonlite::validate(content)) {
    return(jsonlite::fromJSON(content))
  } else {
    return(NULL)
  }
}

