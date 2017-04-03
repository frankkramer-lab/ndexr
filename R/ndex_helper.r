################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 25 January 2017 by Auer
##     
## Description:
##   Some helper function, that are usefull, but don't really fit anywhere else
################################################################################


#' Adds Parameters to an url
#' 
#' Encodes a given parameter within the url accordingly to the parameter configuration for the api. 
#' 
#' @note This function is internal.
#' 
#' @details The single parameter definitions are given as list by the "params" parameter. Each parameter is defined by a method, and, if applicable, a tag, a default value and/or an optional flag.
#' There are three keywords defining the method: replace, append or parameter.
#' 
#' replace: The String defined by "tag" can be found within the url and will be replaced by the given value of the parameter. E.g. the tag "#NETWORKID#" in the url "/network/#NETWORKID#/provenance" is replaced by a value (e.g. "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee") given as network id, which leads to the url "/network/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/provenance". 
#' 
#' append: The given value of the parameter is appended to an url. Therefore the order of the parameters in the params definition is used. E.g. the url "/network/search" and the given values for "start" = 0 and "size" = 100 generates the following url: "/network/search/0/100"
#' 
#' parameter: Encodes the given parameters as url parameter using the specified tag as parameter descriptor. E.g. a parameter with the tag "username" and the value "SomeName" is encoded in the url "/user" as follows: "/user?username=SomeName"
#' 
#' It is also possible to set parameter as optional (except for replace), or define default values. Values are assigned to the parameters using the parameter name in the ... parameter.
#'  
#' @param url character
#' @param params (nested) list; "params" section of a api function definition in the api configuration (See \link{ndex.conf})
#' @param ... parameters defined by name used in the config
#' 
#' @return URL with encoded parameters as character
#' 
#' @examples 
#' ## replace
#' url = "http://en.wikipedia.org/#NETWORKID#/index.php"
#' params = list(    network=list(    tag="#NETWORKID#", method="replace"))
#' values = c(network='aaaa-bb-cc-dddddd', bla='This is not used!')
#' ndexr:::ndex.helper.encodeParams(url, params=params, values)
#' ## "http://en.wikipedia.org/aaaa-bb-cc-dddddd/index.php"
#' 
#' params = list(    network=list(    tag="#NETWORKID#", method="replace", default="xxxx-xx-xx-xxxxxx"))
#' values = c(bla='This is not used!')
#' ndexr:::ndex.helper.encodeParams(url, params=params, values)
#' ## "http://en.wikipedia.org/xxxx-xx-xx-xxxxxx/index.php"
#'  
#' ## parameter
#' url = "http://en.wikipedia.org/w/index.php"
#' params = list(    network=list(    tag="network", method="parameter"))
#' values = c(network='aaaa-bb-cc-dddddd', bla='This is not used!')
#' ndexr:::ndex.helper.encodeParams(url, params=params, values)
#' ## "http://en.wikipedia.org/w/index.php?network=aaaa-bb-cc-dddddd"
#'   
#' values = c(bla='This is not used!')
#' params = list(    network=list(    tag="network", method="parameter", optional=TRUE))
#' ndexr:::ndex.helper.encodeParams(url, params=params, values)
#' ## "http://en.wikipedia.org/w/index.php"
#'   
#' params = list(    network=list(    tag="network", method="parameter", default="xxxx-xx-xx-xxxxxx"))
#' ndexr:::ndex.helper.encodeParams(url, params=params, values)
#' ## "http://en.wikipedia.org/w/index.php?network=xxxx-xx-xx-xxxxxx"
#'   
#' ndexr:::ndex.helper.encodeParams(url, params=params, values)
#' values = c(network='aaaa-bb-cc-dddddd', bla='This is not used!')
#' ## "http://en.wikipedia.org/w/index.php?network=aaaa-bb-cc-dddddd"
#'   
#' ## append
#' url = "http://en.wikipedia.org/w/index.php"
#' params = list(    network=list(    method="append"))
#' values = c(network='aaaa-bb-cc-dddddd', bla='This is not used!')
#' ndexr:::ndex.helper.encodeParams(url, params=params, values)
#' ## "http://en.wikipedia.org/w/index.php/aaaa-bb-cc-dddddd"
#'   
#' values = c(bla='This is not used!')
#' params = list(    network=list(    method="append", optional=TRUE))
#' ndexr:::ndex.helper.encodeParams(url, params=params, values)
#' ## "http://en.wikipedia.org/w/index.php"
#'   
#' params = list(    network=list(    method="append", default="xxxx-xx-xx-xxxxxx"))
#' ndexr:::ndex.helper.encodeParams(url, params=params, values)
#' ## "http://en.wikipedia.org/w/index.php/xxxx-xx-xx-xxxxxx"
#'   
#' values = c(network='aaaa-bb-cc-dddddd', bla='This is not used!')
#' ndexr:::ndex.helper.encodeParams(url, params=params, values)
#' ## "http://en.wikipedia.org/w/index.php/aaaa-bb-cc-dddddd"
ndex.helper.encodeParams = function(url, params, ...){
  urlParamAppend = c()
  urlParamKeyValue = c()
  paramValues = c(...)

  for(curParamName in names(params)){
      curParam = params[[curParamName]]
      method = curParam$method
      if(method == "replace"){
        curParamValue = NULL
        if(curParamName %in% names(paramValues)) curParamValue = paramValues[curParamName]
        else if(! is.null(curParam$default)) curParamValue = curParam$default
        else stop(paste0('Helper: Encode Parameter: Parameter "',curParamName,'" has neither a value nor a default value!'))
        
        curParamTag = curParam$tag
        url = gsub(curParamTag, curParamValue, url)
      }else if(method == "append"){
        curParamValue = NULL
        if(curParamName %in% names(paramValues)) curParamValue = paramValues[curParamName]
        else if(! is.null(curParam$default)) curParamValue = curParam$default
        else if(curParam$optional==TRUE) next
        else stop(paste0('Helper: Encode Parameter: Parameter "',curParamName,'" has neither a value nor a default value, nor is optional!'))
        
        urlParamAppend = c(urlParamAppend, curParamValue)              
      }else if(method == "parameter"){
        curParamValue = NULL
        if(curParamName %in% names(paramValues)) curParamValue = paramValues[curParamName]
        else if(! is.null(curParam$default)) curParamValue = curParam$default
        else if(curParam$optional==TRUE) next
        else stop(paste0('Helper: Encode Parameter: Parameter "',curParamName,'" has neither a value nor a default value, nor is optional!'))
        
        curParamTag = curParam$tag
        urlParamKeyValue = c(urlParamKeyValue, paste(curParamTag, curParamValue, sep='='))
      }else{
          stop(paste0('Helper: Encode Parameter: No method for encoding specified for parameter "',curParamName,'" [',url,']'))
      }
  }
  
  if(length(urlParamAppend)>0) url = paste0(url, paste0('/',urlParamAppend ,collapse = ''))
  if(length(urlParamKeyValue)>0) url = paste0(url,"?",paste0(urlParamKeyValue, collapse='&'))
  return(url)
}


#' Handles the http server response 
#' 
#' This function handles the response from a server. If some response code different from success (200) is returned, the execution stops and the reason is shown.
#' @note This function is internal.
#' 
#' @param response object of class response (httr)
#' @param description character; description of the action performed
#' @param verbose logical; whether to print out extended feedback
#' 
#' @return returns the given respons, if it doesn't contain any HTTP error 
#' 
#' @examples
#' ndexr:::ndex.helper.httpResponseHandler(httr::GET('http://www.ndexbio.org'), 'Tried to connect to NDEx server', TRUE)
ndex.helper.httpResponseHandler <- function(response, description, verbose=FALSE){
    if(missing(response) || is.null(response)){
        stop(paste0('ndex.helper.httpResponseHandler: No server response',description))
    }
    if( !('response' %in% class(response))){
        stop(paste0('ndex.helper.httpResponseHandler: Parameter response does not contain response object!\nResponse:\n',response))
    }
      if('status_code' %in% names(response)){
        if(response$status_code == 200){          ## Success: (200) OK
            if(verbose) message(description, "\nServer is responding with success! (200)",  sep='')
        } else if(response$status_code == 201){          ## Success: (201) OK/Created
              if(verbose) message(description, "\n\tServer is responding with success!\n\t(201) <object creation>",  sep='')
        } else if(response$status_code == 202){          ## Success: (202) OK
              if(verbose) message(description, "\n\tServer is responding with success!\n\t(202) <asynchronized function>",  sep='')
        } else if(response$status_code == 204){          ## Success: (204) OK
              if(verbose) message(description, "\n\tServer is responding with success!\n\t(204) <object modification or deletion>",  sep='')
        } else if(response$status_code == 220){          ## Success: (220) Accepted
              if(verbose) message(description, "\n\tServer is responding with success!\n\t(220) <request accepted>",  sep='')
        } else if(response$status_code == 400){   ## Client error: (400) Bad Request/User unknown
          stop(paste(description, "\n\tBad Request/User unknown! (400)\n"))
        } else if(response$status_code == 401){   ## Client error: (401) Unauthorized
            stop(paste(description, "\n\tUser is not authorized! (401)\n"))
        } else if(response$status_code == 404){   ## Not found error: (404) Page Not Found
            stop(paste(description, "\n\tPage not found! (404)\n\tURL: [",response$url,"]"))
        } else if(response$status_code == 409){   ## Not found error: (409) NDEx_Duplicate_Object_Exception
            content = jsonlite::fromJSON(content(response, as='text', encoding='UTF-8'))
            stop(paste(description, "\n\tNDEx_Duplicate_Object_Exception (409)\n\t",content$message,"\n\tURL: [",response$url,"]"))
        } else if(response$status_code == 500){   ## Server error: (500) Internal Server Error
            error_content = httr::content(response)
            stop(paste(description, "Some internal server error occurred (500):", '\n[errorCode]', error_content$errorCode, '\n[message]', error_content$message, '\n[stackTrace]', error_content$stackTrace, '\n[timeStamp]', error_content$timeStamp, '', sep='\n'))
        } else {   ## Other status
            if(verbose) message(description, "\nServer is responding with unknown status code [",response$status_code, "]", sep='')
        }
    }
    return(response)
}

#' Get the Api configuration for a function
#' 
#' This function extracts the function definition from the ndex configuration within a ndex-connection object.
#' It follows the given path down the list.
#' @note This function is internal.
#' 
#' @param ndexcon object of class NDEXConnection \code{\link{ndex.connect}}
#' @param apiPath character; paht to follow in the nested list
#' 
#' @return configuration of the function
#' 
#' @examples
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' ## Get the function definition for ndex.network.get.summary
#' ## ndex.conf[[ndex.conf$defaultVersion]]$api$network$summary$get
#' ndexr:::ndex.helper.getApi(ndexcon, 'network$summary$get')
ndex.helper.getApi <- function(ndexcon, apiPath){
    if(is.null(ndexcon)||is.null(ndexcon$ndexConf)||is.null(ndexcon$ndexConf$api)){
        stop('API: No or no valid API definition found within the ndex.connection!')
    }
    version = ndexcon$ndexConf$version
    cur = ndexcon$ndexConf$api
    curPath = c()
    for(word in unlist(strsplit(apiPath,'$', fixed = TRUE))){
        curPath = c(curPath, word)
        cur = cur[[word]]
        if(is.null(cur)) stop('API: The method "',paste0(curPath, collapse='->'), ' is not defined for this API (version: ', version, ')')
    }
    return(cur)
}