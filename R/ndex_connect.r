##Initialize internal environment to store package-specific stuff, of no value to user
NDEx.env <- new.env(hash=T)

#' Connect to NDEx REST API
#' 
#' @param baseroute URL of NDEx REST server
#' @param username character username
#' @param password character password
#' @return returns nothing; RCurl options object for authentication is stored in the special environment and reused with the queries
#' @export
ndex.connect <- function(username, password, baseroute='http://dev.ndexbio.org:8080/ndexbio-rest'){
  if(missing(username) || missing(password)) stop("ndex.connect: Username or password not supplied")
  if(grepl("/$", baseroute)) baseroute <- sub("/$", "", baseroute)
  
  ##Attempt authentication
  auth_response <- getURL(paste0(baseroute, "/users/authenticate/", username, "/", password))
  try(auth_response <- fromJSON(auth_response), silent = TRUE)
  if(is.list(auth_response)){
    ##Authentication successful (JSON with user data was returned)
    ndex.opts <- curlOptions(userpwd=paste0(username, ":", password), httpauth = 1L)
    ##Store RCurl options in the internal environment; reuse for other REST queries which require authentication
    assign('ndex.opts', value=ndex.opts, envir=NDEx.env)
    cat("Connected (user ID ", auth_response$id, ")\n",  sep='')
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