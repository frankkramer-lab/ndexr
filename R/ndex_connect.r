##Author: Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##Created: 1 June 2014
## Basic functions to connect and query NDEX API

##Initialize internal environment to store package-specific stuff, of no value to user
NDEx.env <- new.env(hash=T)
##Set default REST server
assign('host', 'http://dev.ndexbio.org:8080/ndexbio-rest', envir=NDEx.env)

#' Connect to NDEx REST API
#' 
#' @param username character username
#' @param password character password
#' @param host (optional) URL of NDEx REST server to be used
#' @return returns nothing; RCurl options object for authentication is stored in the special environment and reused with the queries
#' @note REST server location can also be set separately using \code{\link{ndex.get.host}}. In this case, supplying host here is not necessary
#' @seealso \code{\link{ndex.get.host}}
#' @export
ndex.connect <- function(username, password, host){
  if(missing(username) || missing(password)) stop("ndex.connect: Username or password not supplied")
  if(missing(host)){
    ##Use host URL stored in internal environment (it may be default)
    host <- ndex.get.host()
  } else{
    ##Use supplied host and store it in internal env
    ndex.set.host(host)
  }
  
  ##Attempt authentication
  auth_response <- getURL(paste0(host, "/users/authenticate/", username, "/", password))
  try(auth_response <- fromJSON(auth_response), silent = TRUE)
  if(is.list(auth_response)){
    ##Authentication successful (JSON with user data was returned)
    ndex.opts <- curlOptions(userpwd=paste0(username, ":", password), httpauth = 1L)
    ##Store RCurl options in the internal environment; reuse for other REST queries which require authentication
    assign('ndex.opts', value=ndex.opts, envir=NDEx.env)
    cat("Connected to REST server ", host, "\nWelcome, ", auth_response$username, "!\n",  sep='')
  } else{
    stop(paste("ndex.connect:", auth_response))
  }
  invisible(TRUE)
}

#' Check status of connection to NDEx REST server
#' @return logical (TRUE if connection is active, FALSE otherwise)
#' @export
ndex.alive <- function(){
  
}


#################################################
##Low-level REST-querying functions

#' Generic GET query to API
#' 
#' @param route Character (route to specific REST query)
#' @param auth Logical: is authentication required?
#' @return JSON response from REST server (it will be handled downstream)
#' @details Simply execute HTTP GET on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' @seealso \code{\link{_ndex_rest_PUT}},  \code{\link{_ndex_rest_POST}},  \code{\link{_ndex_rest_DELETE}}
#' @examples
#' \dontrun{ndex_rest_GET("/networks/api")}
ndex_rest_GET <- function(route, auth=TRUE){
  url <- paste0(ndex.get.host(), route)
  if(auth){
    auth.opts <- NDEx.env$ndex.opts
    if(is.null(auth.opts)) stop("Authentication required!")
  } else{
    auth.opts <- curlOptions(httpauth = 1L)
  }
  auth.opts$verbose <- TRUE
  content <- getURL(url, .opts=auth.opts)
  return(content)
}

#' Generic PUT query to API
#' 
#' @param route Character (route to specific REST query)
#' @param ... Whatever data to be supplied with query
#' @param auth Logical: is authentication required?
#' @return JSON response from REST server (it will be handled downstream)
#' @details Simply execute HTTP PUT on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' Making sure the data is well-formed is also the job of calling function
#' @seealso \code{\link{_ndex_rest_GET}},  \code{\link{_ndex_rest_POST}},  \code{\link{_ndex_rest_DELETE}}
#' @examples
#' ##TBD
ndex_rest_PUT <- function(route, ..., auth=TRUE){
  url <- paste0(ndex.get.host(), route)
  if(auth){
    auth.opts <- NDEx.env$ndex.opts
    if(is.null(auth.opts)) stop("Authentication required!")
  } else{
    auth.opts <- curlOptions(httpauth = 1L)
  }
  data <- list(...)[[1]]
  
  rdata <- charToRaw(data)
  
  h = basicTextGatherer()
  h$reset()
  curlPerform(url = url,
              httpheader=c('Content-Type' = "application/json"),
              customrequest = "PUT",
              readfunction=rdata,
              infilesize = length(rdata), upload=TRUE,
              writefunction = h$update,
              .opts = auth.opts, verbose=TRUE)
  
  content = h$value()
  
  #content <- httpPUT(url, content=data , .opts=auth.opts)
  return(content)
}


#' Generic POST query to API
#' 
#' @param route Character (route to specific REST query)
#' @param ... Whatever data to be supplied with query
#' @param auth Logical: is authentication required?
#' @return JSON response from REST server (it will be handled downstream)
#' @details Simply execute HTTP PUT on URL host/route and fetch whatever data REST server returns 
#' Making sure the route is well-formed is the job of calling function
#' Making sure the data is well-formed is also the job of calling function
#' @seealso \code{\link{_ndex_rest_GET}},  \code{\link{_ndex_rest_PUT}},  \code{\link{_ndex_rest_DELETE}}
#' @examples
#' ##TBD
ndex_rest_POST <- function(route, ..., auth=TRUE){
  url <- paste0(ndex.get.host(), route)
  if(auth){
    auth.opts <- NDEx.env$ndex.opts
    if(is.null(auth.opts)) stop("Authentication required!")
  } else{
    auth.opts <- curlOptions(httpauth = 1L)
  }
  dotargs <- list(...)
  data <- dotargs[[1]]
  
  h = basicTextGatherer()
  h$reset()
  curlPerform(url = url,
              postfields = data,
              httpheader = c('Content-Type' = "application/json"),
              writefunction = h$update,
              .opts=auth.opts)
  
  content = h$value()
  return(content)
}

#################################################
##Get/set the REST server URL

#' Set NDEx REST server URL
#' 
#' @param host String with URL to NDEx REST server (currently, default will be \link{http://dev.ndexbio.org:8080/ndexbio-rest})
#' @return returns TRUE invisibly
#' @seealso \code{\link{ndex.get.host}}
#' @export
ndex.set.host <- function(host){
  if(missing(host)) {
    host <- ndex.get.host()
    warning(sprintf("Host URL not supplied. Default host will be set: %s"), host)
  }
  if(!is.character(host)) stop("ndex.set.host: string expected as an input")
  ##Clean up a little (to avoid malformed queries)
  if(grepl("/$", host)) host <- sub("/$", "", host)
  
  ##Check if host is alive
  if(!url.exists(host)) stop(sprintf("Host %s does not exist"), host)
  
  assign('host', host, envir=NDEx.env)
  invisible(TRUE)
}

#' Get NDEx REST server URL
#' 
#' @return REST server URL as a string
#' @seealso \code{\link{ndex.set.host}}
#' @export
ndex.get.host <- function(){
  return(get('host', envir=NDEx.env))
}
