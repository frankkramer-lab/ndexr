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
#' Encodes the given parameter and adds it to the url. The methods can be 'asParams', 'asPath' and 'withinURL'.
#' The methods are chosen automatically depending on the provided parameter:
#' if only values are provided, the values are encoded as url (e.g. "url/value1/value2/...")[encoding='asPath']
#' if the params are unnamed and values are provided, it is encoded as params (e.g. "url?param1=value1&param2=value2&...")[encoding='asParams']
#' if the params are named an no values are provided, it is encoded as params (e.g. "url?param1=value1&param2=value2&...")[encoding='asNamedParams']
#' if named params and values are given, the value of params is used to replace it in the url with the corresponding value (e.g. url="abc.de/#test#/else", params=(bla='#test#') and value=('blubb') becomes to "abc.de/blubb/else")[encoding='withinURL']
#' 
#' @param url character
#' @param params (named) character vector;
#' @param values character vector (optional if params are named);
#' @param encoding character (optional); Method to encode the parameter: 'asParams', 'asNamedParams', 'asPath', 'withinURL';
#' @return URL with encoded parameters as character
#' @note params and values must have the same length
#' @examples 
#' \dontrun{
#' url = "http://en.wikipedia.org/w/index.php"
#' values = c("Train", "5", "90", "history")
#' ndex.helper.encodeParams(url, values=values)
#' #[1] "http://en.wikipedia.org/w/index.php/Train/5/90/history"
#' 
#' params = c("title", "limit", "offset", "action") 
#' namedParams = c(title="Train", limit="5", offset="90", action="history")
#' ndex.helper.encodeParams(url, params=params, values=values)
#' ndex.helper.encodeParams(url, params, values)          ## same as above, but shorter
#' ndex.helper.encodeParams(url, params=namedParams)      ## same as above, but with named params
#' ndex.helper.encodeParams(url, namedParams)             ## same as above, but shorter
#' #[1] "http://en.wikipedia.org/w/index.php?title=Train&limit=5&offset=90&action=history"
#' 
#' url = "http://en.wikipedia.org/w/index.php/#Train#/somethingElse/#Number#"
#' namedParams = c(train="#Train#", someNumber="#Number#")
#' values = c("ICE200", 12345)
#' ndex.helper.encodeParams(url, params=namedParams, values=values)
#' ndex.helper.encodeParams(url, namedParams, values)
#' #[1] "http://en.wikipedia.org/w/index.php/ICE200/somethingElse/12345"
#' }
ndex.helper.encodeParams = function(url, params, values, encoding){
  # pn=pv, v   withinurl
  # pn=pv      asnamedparams
  # pv, v      asparams
  # v          aspath
  
  if(missing(encoding)){
    if(missing(params)){              ## no params
      if(missing(values)){            ## no values
        encoding = 'url'                  ## nothing to encode!  
      }else{                          ## with values
        encoding = 'aspath'               ## only values are given
      }
    }else{                            ## with params
      if(! is.null(names(params))){   ## params with names
        if(missing(values)){          ## no values
          encoding = 'asnamedparams'           ## only name-value pairs
        }else{                        ## with values
          encoding = 'withinurl'          ## name-value pairs and extra value
        }
      }else{                          ## params without names
        if(missing(values)){          ## no values
          encoding = 'url'           ## only params; nothing to encode!
        }else{                        ## with values
          encoding = 'asparams'          ## param-value pairs
        }
      }
    }
  }
  
  result = ''
	if(tolower(encoding)=='asparams') {
	  result = paste0(url,"?",paste(params,values, sep = '=', collapse = '&'))
	}else if(tolower(encoding)=='asnamedparams') {
	  result = paste0(url,"?",paste(names(params),params, sep = '=', collapse = '&'))
	}else if(tolower(encoding)=='withinurl') {
	  result = url
	  paramNames = names(params)
	  names(params) = NULL
	  for(i in 1:length(params)) {
	    result = gsub(params[i],values[i], result)
	  }
	}else if(tolower(encoding)=='aspath') {
	  result = paste0(url,"/",paste0(values, collapse = '/'))
	}else{
	  result = url
	}
	return(result)
}


#' Handles the http server response 
#' 
#' This function handles the response from a server. If some response code different from success (200) is returned, the execution stops and the reason is shown.
#' 
#' @param response object of class response (httr)
#' @param description character; description of the action performed
#' @param verbose logical; whether to print out extended feedback
#' @examples
#' \dontrun{
#'  ndex.helper.httpResponseHandler(httr::GET('http://www.ndexbio.org'), 'Tried to connect to NDEx server', T)
#'  }
ndex.helper.httpResponseHandler <- function(response, description, verbose=F){
	if( !('response' %in% class(response))){
		stop('ndex.helper.httpResponseHandler: Parameter response does not contain response object')
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
		} else if(response$status_code == 500){   ## Server error: (500) Internal Server Error
			error_content = httr::content(response)
			stop(paste(description, "Some internal server error occurred (500):", '\n[errorCode]', error_content$errorCode, '\n[message]', error_content$message, '\n[stackTrace]', error_content$stackTrace, '\n[timeStamp]', error_content$timeStamp, '', sep='\n'))
		} else {   ## Other status
			if(verbose) message(description, "\nServer is responding with unknown status code [",response$status_code, "]", sep='')
		}
	}
	return(response)
}

ndex.helper.getApi <- function(ndexcon, apiPath){
	if(is.null(ndexcon)||is.null(ndexcon$apiConfig)||is.null(ndexcon$apiConfig$api)){
		stop('API: No or no valid API definition found within the ndex.connection!')
	}
	version = ndexcon$apiConfig$version
	cur = ndexcon$apiConfig$api
	curPath = c()
	for(word in unlist(strsplit(apiPath,'$', fixed = T))){
		curPath = c(curPath, word)
		cur = cur[[word]]
		if(is.null(cur)) stop('API: The method "',paste0(curPath, collapse='->'), ' is not defined for this API (version: ', version, ')')
	}
	return(cur)
}