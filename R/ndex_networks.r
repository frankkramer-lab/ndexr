##Authors:
#   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
#   Dexter Pratt [depratt@ucsd.edu]
##Created: 6 June 2014
# Contains functions to search and retrieve networks

#' Search networks in NDEx (by description)
#' 
#' @param searchString string by which to search
#' @param accountName string; constrain search to networks administered by this account
#' @param skip how many networks to skip
#' @param top how many networks to show
#' @return Data frame with network information: ID, name, whether it is public, edge and node count; source and format of network
#' @note Search strings may be structured
#' @examples \dontrun{ndex.find.networks("calmodulin")}
#' @export
ndex.find.networks <- function(searchString, accountName, skip = 0, top = 10){
  # searchType was an NDEx Beta feature but is not supported in v1.0. 
  # An equivalent functionality may return in future versions.
  # Dexter 10/30/14
  # #' @param searchType string; type of search (should be one of "exact-match", "contains", "begins-with")
  # searchType <- match.arg(searchType, choices=c("exact-match", "contains", "begins-with"))
  
  ##Form JSON to post
  query <- toJSON(list(searchString=searchString, accountName=accountName skip=skip, top=top))
  
  ##Form route
  route <- "/networks/search"
  is.authorized <- exists('ndex.opts', envir=NDEx.env)
  
  ##Get stuff
  response_json <- ndex_rest_POST(route=route, query, auth=is.authorized)
  
  response <- fromJSON(response_json)
  ##Retrieve necessary data fields
  out <- lapply(response, json_parse_network_metadata)
  out <- do.call(rbind, out)
  
  return(out)
}

#' Get network metadata by ID
#' 
#' @param network_id unique ID of the network
#' @param json logical; whether to return JSON (TRUE) or convert it to data frame. Default FALSE 
#' @return Complete JSON response or Data frame with network metadata: ID, name, whether it is public, edge and node count; source and format of network
#' @export
ndex.get.network.metadata <- function(network_id, json=FALSE){
  route <- paste0("/networks/", network_id)
  response <- ndex_rest_GET(route)
  if(json) return(response)
  else return(json_parse_network_metadata(response))
}

#' Get network edges by ID
#' 
#' @param network_id unique ID of the network
#' @return \code{\link{ndexgraph}} object
#' @details Uses getEdges (this procedure will return complete network with all elements)
#' Nodes use primary ID of the base term ('represents' element)
#' Edges use primary ID of the base term ('predicate', or 'p' element)
#' Mapping table for the nodes is retrieved ('alias' and 'related' terms) to facilitate conversions/data mapping
#' @note Currently fetches all edges; may be suboptimal for huge networks
#' @export
ndex.get.network <- function(network_id, json=FALSE){
  ##First get the metadata
  m <- ndex.get.network.metadata(network_id)
  count <- m$edge_count
  
  ##For now, just fetch all edges
  route <- paste0("/networks/", network_id, "/edges/0/", count)
  ejson <- ndex_rest_GET(route)
  
  nw <- fromJSON(ejson)
  
  ##Retrieve all namespaces
  nslist <- nw$namespaces
  namespaces <- jsonlist2df(nslist)
  
  ##Retrieve all base terms
  termlist <- nw$terms
  terms <- jsonlist2df(termlist)
  terms$namespace_name <- namespaces$prefix[match(terms$namespace, namespaces$jdexId)]
  
  ##Get node data frame
  nodelist <- nw$nodes
  nodes <- do.call(rbind, lapply(nodelist, function(x){c(x$name, x$id, x$represents)}))
  nodes <- data.frame(id=names(nodelist), nodes, stringsAsFactors = FALSE)
  colnames(nodes) <- c('node_id', 'name', 'id', 'term_id')
  ##Append information from the terms: namespace name and ID of 'representing' term in its namespace
  terminfo_nodes <- terms[as.character(nodes$term_id), c('name', 'namespace', 'namespace_name')]
  colnames(terminfo_nodes)[1] <- 'ref'
  nodes <- cbind(nodes, terminfo_nodes)
  
  ##Get aliases and related terms for the nodes formatted
  node.aliases <- lapply(nodelist, '[[', 'aliases')
  alias_df <- unlist2df(node.aliases, names=c('node_id', 'ref_id'))
  alias_df$type <- 'Alias'
  node.related <- lapply(nodelist, '[[', 'relatedTerms')
  related_df <- unlist2df(node.related, names=c('node_id', 'ref_id'))
  related_df$type <- 'Related'
  node.annot <- rbind(alias_df, related_df)
  terminfo_annot <- terms[as.character(node.annot$ref_id), c('name', 'namespace', 'namespace_name')]
  colnames(terminfo_annot)[1] <- 'ref'
  node.annot <- cbind(node.annot, terminfo_annot)
  
  ##Retrieve all edges
  edgelist <- nw$edges
  edges <- jsonlist2df(edgelist)
  ##Append information from the terms: namespace name and ID of predicate in this namespace
  terminfo_edges <- terms[as.character(edges$p), c('name', 'namespace', 'namespace_name')]
  colnames(terminfo_edges)[1] <- 'edge_desc'
  edges <- cbind(edges, terminfo_edges)
  
  out <- new('ndexgraph',
             nodes = nodes,
             edges = edges,
             node_annot = node.annot,
             metadata = m,
             name = m$network_name,
             id = m$network_id)
}


##########################################################
##Auxiliary format conversion code (JSON -> list -> data frame)
#' Misc operations with lists (helps converting JSON to tabular formats)
#'
#' @param l list
#' @name aux_list
NULL

#' @rdname aux_list
#' 
#' @details \code{jsonlist2df} flattens JSON-derived list and returns a vector. NULLs are replaced with emply string (""); non-vector elements and vector elements with length > 1 are dropped
#' @examples jsonlist2vector(list(a=1, b=NULL, c=1:3, d='v'))
jsonlist2vector <- function(l){
  if(!is.list(l)) stop("list  expected")
  l <- lapply(l, function(x){if(is.null(x)) return('') else return(x)})
  l[sapply(l, is.list)] <- ''
  l[sapply(l, length) > 1] <- NULL
  cc <- unlist(l)
  return(cc)
}

#' @rdname aux_list
#' 
#' @details \code{jsonlist2df} converts JSON-derived lists of the same structure into a data frame. each row corresponds to first order element in l
#'  for each row, NULLs are replaced with emply string (""); non-vector elements and vector elements with length > 1 are dropped
jsonlist2df <- function(l){
  if(!is.list(l)) stop("list  expected")
  ll <- lapply(l, jsonlist2vector)
  ll_lengths <- sapply(ll, length)
  if(dim(table(ll_lengths)) != 1) stop("jsonlist2df: Elements of list have different structure, impossible to convert to a data frame")
  df <- do.call(rbind, ll)
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  return(df)
}

#' @rdname aux_list
#' @param names character vector of length 2 with column names for output
#' @details \code{unlist2df} turns named list into a data frame where 1st column has names of list elements and 2nd has corrsponding values. List should be named and should contain vectors. Non-vector elements will be dropped
unlist2df <- function(l, names=c('key', 'value')){
  if(!is.list(l)) stop("list  expected")
  ##Drop non-vector elements
  l[!(sapply(l, is.vector))] <- NULL
  keys <- rep(names(l), sapply(l, length))
  values <- unlist(l)
  out <- data.frame(keys, values, stringsAsFactors=FALSE)
  names(out) <- names
  return(out)
}

#' Convert JSON of network metadata to data frame
#' 
#' @param nd list with network metadata information (JSON or parsed JSON)
#' @return Data frame with network information: ID, name, whether it is public, edge and node count; source and format of network
json_parse_network_metadata <- function(nd){
  if(!is.list(nd)) nd <- fromJSON(nd)
  if(any(sapply(nd, is.null))) nd <- lapply(nd, function(x){if(is.null(x)) return('') else return(x)})
  out <- data.frame(network_id = nd$id,
                    network_name = nd$name,
                    node_count = nd$nodeCount,
                    edge_count = nd$edgeCount,
                    isPublic = nd$isPublic,
                    source = nd$metadata['Source'],
                    format = nd$metadata['Format'],
                    stringsAsFactors=FALSE)
  return(out)
}
