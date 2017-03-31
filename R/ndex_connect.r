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
#' @param host character (default: "ndexConf$connection$host"); Host address of NDEx server; By default the url set in ndexConf$defaults$connection$host is used. ("http://www.ndexbio.org")
#' @param apiPath character (default: "ndexConf$connection$api"); URL path of the REST api; By default the url set in ndexConf$defaults$connection$api is used. ("http://www.ndexbio.org")
#' @param ndexConf config object (nested list, default: ndex.conf); Configuration of NDEx REST server; Set in ndex.conf (set in ndex_api_config.r or ndex_api_config.yml): It contains specifications for NDEx server api version 1.3 and 2.0. The default api is specified by 'defaultVersion'
#' @param verbose logical (optional); whether to print out extended feedback 
#' 
#' @return returns object of class NDEXConnection which stores options, authentication and api configuration if successfull, NULL otherwise
#' 
#' @examples
#' ndexcon = ndex.connect()   ## log in anonymously
#' ndexcon = ndex.connect(verbose=TRUE)   ## same as above with extended feedback
#' \dontrun{
#' ndexcon = ndex.connect('user','password')   ## log in with credentials
#' ndexcon = ndex.connect(host='localhost:8765')   ## running some NDEx server locally
#' ndexcon = ndex.connect(ndexConf=ndex.conf$Version_2.0)   ## manually change the api and connection configuration
#' }
#' @seealso  \code{\link{ndex.conf}}, \code{\link{updateConfigFromYaml}}
#' @export
ndex.connect <- function(username, password, host = "ndexConf$connection$host", apiPath = 'ndexConf$connection$api', ndexConf=ndex.conf, verbose = FALSE){

  ##Check parameters and set defaults by config
  credentials = TRUE
  if(missing(username) || missing(password)){
    message("ndex.connect: Connecting anonymously - username or password not supplied")
    credentials = FALSE
  }
  
  if(missing(ndexConf)||('defaultVersion' %in% names(ndexConf))){
    defaultVersion = ndexConf$defaultVersion
    ndexConf=ndexConf[[defaultVersion]]
    if(verbose) message(paste("ndex.connect: ndexConf not specified, using default version ", ndexConf$version, " [", defaultVersion, "]" ))
  } else {
    if(verbose) message(paste("ndex.connect: Using ndexConf for version ", ndexConf$version))
  }
  
  if(missing(apiPath)){
      apiPath = ndexConf$connection$api
      if(verbose) message(paste("ndex.connect: apiPath not specified, using default: [", apiPath, "]" ))
  }
  
  if(missing(host)){
      host = paste0(ndexConf$connection$host, apiPath)
      if(verbose) message(paste("ndex.connect: Host not specified, using default: [", host, "]" ))
  } else {
      host = paste0(host, apiPath)
      if(verbose) message(paste("ndex.connect: Using host: [", host, "]" ))
  }
  
  ##Setup server connection, with and without credentials
  ##Check if server is available
  auth_param = NULL
  api = ndex.helper.getApi(list(ndexConf=ndexConf), 'serverStatus')
  route <- ndex.helper.encodeParams(api$url, api$params)
  url <- paste0(host, route)
  log_txt = paste0("ndex.connect: Tried to check the server status of [", host, "]")

  ##Try to connect to the server; throws error if something went wrong
  response = ndex.helper.httpResponseHandler(httr::GET(url=url, config=auth_param), 
                                             log_txt, 
                                             verbose)
  if(verbose) message(paste("ndex.connect: Server response: ", response))
  
  ##Checkt the provided credentials
  if (credentials){
      api = ndex.helper.getApi(list(ndexConf=ndexConf), 'user$authenticate')
      route <- ndex.helper.encodeParams(api$url, api$params)
      url <- paste0(host, route)
      auth_param = httr::authenticate(username, password)
      log_txt = paste0("ndex.connect: Tried to autheticate user: ", username,' @ ',url)
      response = ndex.helper.httpResponseHandler(httr::GET(url=url, config=auth_param), 
                                               log_txt, 
                                               verbose)
      if(verbose) message(paste("ndex.connect: Server response: ", response))    
  }
                         
  
  ##Create ndexcon object
  ndexcon = list(anonymous=TRUE, host=host, ndexConf=ndexConf, verbose=verbose)
  if(credentials) {
    ndexcon$anonymous = FALSE
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
#' @note This function is internal.
#' 
#' @param ndexcon object of class NDEXConnection \code{\link{ndex.connect}}
#' @param route Character (route to specific REST query)
#' @param raw Specifies if server response should be returned in raw, or if jsonlite::fromJSON is called first. Defaults to FALSE.
#' 
#' @return JSON response from REST server, NULL if no valid JSON was received. if parameter raw is TRUE, the raw response is returned without a call to jsonlite::fromJSON.
#' 
#' @details Simply execute HTTP GET on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' @seealso \code{\link{ndex_rest_GET}},  \code{\link{ndex_rest_POST}},  \code{\link{ndex_rest_PUT}} and \code{\link{ndex_rest_DELETE}}
#' 
#' @examples
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' \dontrun{
#' ndex_rest_GET(ndexcon, "/networks/api")
#' }
ndex_rest_GET <- function(ndexcon, route, raw = FALSE){
  url <- paste0(ndexcon$host, route)
  auth <- NULL
  if(! ndexcon$anonymous) auth = httr::authenticate(ndexcon$username, ndexcon$password)
  
  try(response <- httr::GET(url, auth))
  ndex.helper.httpResponseHandler(response, paste("GET: [", url, "]"), ndexcon$verbose)
  content <- content(response, as='text', encoding='UTF-8')
  
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
#' @note This function is internal.
#' 
#' @param ndexcon object of class NDEXConnection \code{\link{ndex.connect}}
#' @param route Character (route to specific REST query)
#' @param data Whatever data to be supplied with query. Should be valid JSON
#' @param multipart Whatever data to be supplied with query. Should be valid JSON
#' @param raw Specifies if server response should be returned in raw, or if jsonlite::fromJSON is called first. Defaults to FALSE.
#' 
#' @return JSON response from REST server, NULL if no valid JSON was received. if parameter raw is TRUE, the raw response is returned without a call to jsonlite::fromJSON.
#' 
#' @details Simply execute HTTP POST on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' Making sure the data is well-formed is also the job of calling function
#' @seealso \code{\link{ndex_rest_GET}},  \code{\link{ndex_rest_POST}},  \code{\link{ndex_rest_PUT}} and \code{\link{ndex_rest_DELETE}}
#' 
#' @examples
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' \dontrun{
#' ndex_rest_POST(ndexcon, "/networks/api", data)
#' ndex_rest_POST(ndexcon, "/networks/api", data, raw=TRUE)
#' ndex_rest_POST(ndexcon, "/networks/api", list(some=data, other=data2), multipart=TRUE)
#' }
ndex_rest_POST <- function(ndexcon, route, data, multipart = FALSE, raw = FALSE){
  url <- paste0(ndexcon$host, route)
  auth <- NULL
  if(! ndexcon$anonymous) auth <- httr::authenticate(ndexcon$username, ndexcon$password)
  encode <- ifelse(multipart, 'multipart', 'json')
  contenttype <- httr::content_type_json()
  if(multipart) contenttype <- httr::content_type('multipart/form-data')
  
  try(response <- httr::POST(url, auth, contenttype, body = data, encode = encode))
  
  ndex.helper.httpResponseHandler(response, paste("POST: [", url, "]\ndata:\n",substring(data, 1, 300),'\n...'), ndexcon$verbose)
  content <- content(response, as='text', encoding='UTF-8')
  
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
#' @note This function is internal.
#' 
#' @param ndexcon object of class NDEXConnection \code{\link{ndex.connect}}
#' @param route Character (route to specific REST query)
#' @param data Whatever data to be supplied with query. Should be valid JSON
#' @param multipart Whatever data to be supplied with query. Should be valid JSON
#' @param raw Specifies if server response should be returned in raw, or if jsonlite::fromJSON is called first. Defaults to FALSE.
#' 
#' @return JSON response from REST server, NULL if no valid JSON was received. if parameter raw is TRUE, the raw response is returned without a call to jsonlite::fromJSON.
#' 
#' @details Simply execute HTTP PUT on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' Making sure the data is well-formed is also the job of calling function
#' @seealso \code{\link{ndex_rest_GET}},  \code{\link{ndex_rest_POST}},  \code{\link{ndex_rest_PUT}} and \code{\link{ndex_rest_DELETE}}
#' 
#' @examples
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' \dontrun{
#' ndex_rest_PUT(ndexcon, "/networks/api", data)
#' ndex_rest_PUT(ndexcon, "/networks/api", data, raw=TRUE)
#' ndex_rest_PUT(ndexcon, "/networks/api", list(some=data, other=data2), multipart=TRUE)
#' }
ndex_rest_PUT <- function(ndexcon, route, data=NULL, multipart = FALSE, raw = FALSE){
    url <- paste0(ndexcon$host, route)
    auth <- NULL
    if(! ndexcon$anonymous) auth <- httr::authenticate(ndexcon$username, ndexcon$password)
    encode <- ifelse(multipart, 'multipart', 'json')
    contenttype <- httr::content_type_json()
    if(multipart) contenttype <- httr::content_type('multipart/form-data')
    
    try(response <- httr::PUT(url, auth, contenttype, body = data, encode = encode))
    
    ndex.helper.httpResponseHandler(response, paste("PUT: [", url, "]\ndata:\n",substring(data, 1, 300),'\n...'), ndexcon$verbose)
    content <- content(response, as='text', encoding='UTF-8')
    
    if(ndexcon$verbose) message('Response:', substring(content, 1, 300), '...', sep = '\n')
    if(raw) return(content)
    if(jsonlite::validate(content)) {
        return(jsonlite::fromJSON(content))
    } else {
        return(NULL)
    }
}


#' Generic DELETE query to API. 
#' 
#' @note This function is internal.
#' 
#' @param ndexcon object of class NDEXConnection \code{\link{ndex.connect}}
#' @param route Character (route to specific REST query)
#' @param raw Specifies if server response should be returned in raw, or if jsonlite::fromJSON is called first. Defaults to FALSE.
#' 
#' @return JSON response from REST server, NULL if no valid JSON was received. if parameter raw is TRUE, the raw response is returned without a call to jsonlite::fromJSON.
#' 
#' @details Simply execute HTTP DELETE on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' @seealso \code{\link{ndex_rest_GET}},  \code{\link{ndex_rest_POST}},  \code{\link{ndex_rest_PUT}} and \code{\link{ndex_rest_DELETE}}
#' 
#' @examples
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' \dontrun{
#' ndex_rest_DELETE(ndexcon, "/networks/api")
#' }
ndex_rest_DELETE <- function(ndexcon, route, raw = FALSE){
  url <- paste0(ndexcon$host, route)
  auth <- NULL
  if(! ndexcon$anonymous) auth = httr::authenticate(ndexcon$username, ndexcon$password)
  
  try(response <- httr::DELETE(url, auth, encode = 'json'))
  ndex.helper.httpResponseHandler(response, paste("DELETE: [", url, "]"), ndexcon$verbose)
  content <- content(response, as='text', encoding='UTF-8')
  
  if(ndexcon$verbose) message('Response:', substring(content, 1, 300), '...', sep = '\n')
  if(raw) return(content)
  if(jsonlite::validate(content)) {
    return(jsonlite::fromJSON(content))
  } else {
    return(NULL)
  }
}

