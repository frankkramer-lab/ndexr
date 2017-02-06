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
#' This functions searches the public networks on an NDEx server for networks containing the supplied search string. T
#' his search can be limited to certain accounts as well as in length.
#' 
#' @param url string
#' @param params character vector;
#' @param values character vector;
#' @return URL with parameters as string
#' @note params and values must have the same length
#' @examples 
#' \dontrun{
#' ndex.helper.UrlAddParams("http://en.wikipedia.org/w/index.php", c("title", "limit", "offset", "action"), c("Train", "5", "90", "history"))
#' # "http://en.wikipedia.org/w/index.php?title=Train&limit=5&offset=90&action=history"
#' }
ndex.helper.UrlAddParams = function(url, params, values){
	paste0(url,"?",paste(params,values, sep = '=', collapse = '&'))
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
		} else if(response$status_code == 401){   ## Client error: (401) Unauthorized
			stop(paste(description, "\n\tUser is not authorized! (401)\n"))
		} else if(response$status_code == 404){   ## Not found error: (404) Page Not Found
			stop(paste(description, "\n\tPage not found! (404)\n\tURL: [",response$url,"]"))
		} else if(response$status_code == 500){   ## Server error: (500) Internal Server Error
			error_content = content(response)
			stop(paste(description, "Some internal server error occurred (500):", '\n[errorCode]', error_content$errorCode, '\n[message]', error_content$message, '\n[stackTrace]', error_content$stackTrace, '\n[timeStamp]', error_content$timeStamp, '', sep='\n'))
		} else {   ## Other status
			if(verbose) message(description, "\nServer is responding with unknown status code [",response$status_code, "]", sep='')
		}
	}
	return(response)
}