##Author: Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##Created: 6 June 2014
# Contains functions to search and retrieve networks

#' Search networks in NDEx (by description)
#' 
#' @param searchString string by which to search
#' @param searchType string; type of search (should be one of "exact-match", "contains", "begins-with")
#' @param skip how many networks to skip
#' @param top how many networks to show
#' @return Data frame with network information: ID, name, whether it is public, edge and node count; source and format of network
#' @note Search strings may be structured
#' @examples \dontrun{ndex.find.networks("calmodulin")}
#' @export
ndex.find.networks <- function(searchString, searchType="contains", skip = 0, top = 10){
  searchType <- match.arg(searchType, choices=c("exact-match", "contains", "begins-with"))
  
  ##Form JSON to post
  query <- toJSON(list(searchString=searchString, skip=skip, top=top))
  
  ##Form route
  route <- paste0("/networks/search/", searchType)
  is.authorized <- exists('ndex.opts', envir=NDEx.env)
  
  ##Get stuff
  response_json <- ndex_rest_POST(route=route, query, auth=is.authorized)
  
  response <- fromJSON(response_json)
  ##Retrieve necessary data fields
  out <- data.frame(network_id = sapply(response, '[[', "id"),
                    network_name = sapply(response, '[[', "name"),
                    node_count = sapply(response, '[[', "nodeCount"),
                    edge_count = sapply(response, '[[', "edgeCount"),
                    isPublic = sapply(response, '[[', "isPublic"),
                    source = sapply(response, function(x){x$metadata['Source']}),
                    format = sapply(response, function(x){x$metadata['Format']}),
                    stringsAsFactors = FALSE)
  
  return(out)
}