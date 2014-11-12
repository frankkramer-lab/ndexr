##Authors:
#   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
#   Dexter Pratt [depratt@ucsd.edu]
##Created: 6 June 2014
# Contains functions to search and retrieve networks

get.network.api <- function(){
  route <- "/network/api"
  response <- ndexr:::ndex_rest_GET(route)
  df <- data.frame(path = sapply(response, `[[`, 'path'),
                   description = sapply(response, `[[`, 'apiDoc'),
                   requestType = sapply(response, `[[`, 'requestType'),stringsAsFactors = FALSE)
  return(df)
}

#' Search networks in NDEx (by description)
#'     network    POST    /network/search/{skipBlocks}/{blockSize}    SimpleNetworkQuery    NetworkSummary[]
#' 
#' @param searchString string by which to search
#' @param accountName string; constrain search to networks administered by this account
#' @param skip how many networks to skip
#' @param top how many networks to show
#' @return Data frame with network information: ID, name, whether it is public, edge and node count; source and format of network
#' @note Search strings may be structured
#' @examples \dontrun{ndex.find.networks("calmodulin")}
#' @export
ndex.find.networks <- function(searchString="", accountName, skipBlocks = 0, blockSize = 10){
  # searchType was an NDEx Beta feature but is not supported in v1.0. 
  # An equivalent functionality may return in future versions.
  # Dexter 10/30/14
  # #' @param searchType string; type of search (should be one of "exact-match", "contains", "begins-with")
  # searchType <- match.arg(searchType, choices=c("exact-match", "contains", "begins-with"))
  
  ##Form JSON to post
  if (missing(accountName)){
    query <- toJSON(list(searchString=searchString))
  } else {
    query <- toJSON(list(searchString=searchString, accountName=accountName))
  }
  
  ##Form route
  route <- sprintf("/network/search/%s/%s", skipBlocks, blockSize)
  #is.authorized <- exists('ndex.opts', envir=NDEx.env)
  
  ##Get a list of NetworkSummary objects
  response <- ndex_rest_POST(route=route, query)
  
  ##Retrieve necessary data fields
  out <- lapply(response, json_parse_network_metadata)
  out <- do.call(rbind, out)
  
  return(out)
}

# def findNetworksAsDataFrame(self, searchString="", accountName=None, skipBlocks=0, blockSize=100): 
#   return pd.DataFrame(self.findNetworks(searchString, accountName, skipBlocks, blockSize))

##########################################################
# Network Functions

#' Get NetworkSummary by Network UUID
#' GET    /network/{networkUUID}       NetworkSummary
#' 
#' @param network_id unique ID of the network
#' @return Complete JSON response or Data frame with network metadata: ID, name, whether it is public, edge and node count; source and format of network
#' @export
ndex.get.network.summary <- function(network_id){
  route <- paste0("/network/", network_id)
  response <- ndex_rest_GET(route)
  return(json_parse_network_metadata(response))
}



#' Get complete network
#' 
#' @param network_id unique ID of the network
#' @return \code{\link{ndexgraph}} object
#' @details Uses getEdges (this procedure will return complete network with all elements)
#' Nodes use primary ID of the base term ('represents' element)
#' Edges use primary ID of the base term ('predicate', or 'p' element)
#' Mapping table for the nodes is retrieved ('alias' and 'related' terms) to facilitate conversions/data mapping
#' @section REST query:
#' This function runs GET query /network/{networkUUID}/asNetwork (returns Network object
#' @export
ndex.get.complete.network <- function(network_id){
  route <- paste0("/network/", network_id, "/asNetwork")
  return(ndex_rest_GET(route))
}

# POST    /network/{networkUUID}/edge/asNetwork/{skipBlocks}/{blockSize}        Network
ndex.get.network.by.edges <- function(networkId, skipBlocks=0, blockSize=100){
  route <- sprintf('/network/%s/edge/asNetwork/%s/%s', networkId, skipBlocks, blockSize)
  return(ndex_rest_GET(route))
}

# POST    /network    Network    NetworkSummary
ndex.save.new.network <- function(network){
  route <- "/network/asNetwork"
  return(ndex_rest_POST(route, network))
}

# POST    /network/asNetwork/group/{group UUID}    Network    NetworkSummary
ndex.save.new.network.for.group <- function(network, groupId){
  route <- paste0("/network/asNetwork/group/", groupId)
  network <- remove.uuid.from.network(network)
  return(ndex_rest_POST(route, network))
}

##  Neighborhood PathQuery
#   POST    /network/{networkUUID}/asPropertyGraph/query    SimplePathQuery    PropertyGraphNetwork    
ndex.get.neighborhood <- function(networkId, searchString="", searchDepth=1){
  route <- sprintf("/network/%s/asNetwork/query", networkId) 
  query <- toJSON(list(searchString=searchString, searchDepth=searchDepth))
  return(ndex_rest_POST(route, query))
}

##########################################################
# PropertyGraphNetwork Functions


#' Get complete network as property graph
#'  
#' @param network_id unique ID of the network
#' @return \code{\link{ndexgraph}} object
#' @details Uses getEdges (this procedure will return complete network with all elements)
#' Nodes use primary ID of the base term ('represents' element)
#' Edges use primary ID of the base term ('predicate', or 'p' element)
#' Mapping table for the nodes is retrieved ('alias' and 'related' terms) to facilitate conversions/data mapping
#' @section REST query:
#' Executes GET query /network/{networkUUID}/asPropertyGraph, returns parsed PropertyGraphNetwork object
#' @examples
#' \dontrun{
#' x <- ndex.get.complete.network.as.property.graph('32733ca9-4e84-11e4-98fb-000c29873918')
#' }
#' @export
ndex.get.complete.network.as.property.graph <- function(network_id){
  route <- paste0("/network/", network_id, "/asPropertyGraph")
  return(ndex_rest_GET(route))
}

#   POST    /network/{networkUUID}/edge/asPropertyGraph/{skipBlocks}/{blockSize}        PropertyGraphNetwork
ndex.get.property.graph.network.by.edges <- function(networkId, skipBlocks=0, blockSize=100){
  route <- sprintf("/network/%s/edge/asPropertyGraph/%s/%s", networkId, skipBlocks, blockSize)
  return(ndex_rest_GET(route))
}

#   POST    /network/asPropertyGraph    PropertyGraphNetwork    NetworkSummary
ndex.save.new.property.graph.network <- function(propertyGraphNetwork){
  route <- "/network/asPropertyGraph"
  propertyGraphNetwork <- ndex.remove.uuid.from.network(propertyGraphNetwork)
  return(ndex_rest_POST(route, propertyGraphNetwork))
}

#   POST    /network/asPropertyGraph/group/{group UUID}    PropertyGraphNetwork    NetworkSummary
ndex.save.new.property.graph.network.for.group <- function(propertyGraphNetwork, groupId){
  route <- sprintf("/network/asPropertyGraph/group/%s", groupId)
  propertyGraphNetwork <- ndex.remove.uuid.from.network(propertyGraphNetwork)
  return(ndex_rest_POST(route, propertyGraphNetwork))
}

##  Neighborhood PathQuery
#   POST    /network/{networkUUID}/asPropertyGraph/query    SimplePathQuery    PropertyGraphNetwork 
ndex.get.neighborhood.as.property.graph <-function(networkId, searchString="", searchDepth=1){
  route <- sprintf("/network/%s/asPropertyGraph/query", networkId) 
  query <- toJSON(list(searchString=searchString, searchDepth=searchDepth))
  return(ndex_rest_POST(route, query))
}

# 
#   nw <- fromJSON(ejson)
#   
#   ##Retrieve all namespaces
#   nslist <- nw$namespaces
#   namespaces <- jsonlist2df(nslist)
#   
#   ##Retrieve all base terms
#   termlist <- nw$terms
#   terms <- jsonlist2df(termlist)
#   terms$namespace_name <- namespaces$prefix[match(terms$namespace, namespaces$jdexId)]
#   
#   ##Get node data frame
#   nodelist <- nw$nodes
#   nodes <- do.call(rbind, lapply(nodelist, function(x){c(x$name, x$id, x$represents)}))
#   nodes <- data.frame(id=names(nodelist), nodes, stringsAsFactors = FALSE)
#   colnames(nodes) <- c('node_id', 'name', 'id', 'term_id')
#   ##Append information from the terms: namespace name and ID of 'representing' term in its namespace
#   terminfo_nodes <- terms[as.character(nodes$term_id), c('name', 'namespace', 'namespace_name')]
#   colnames(terminfo_nodes)[1] <- 'ref'
#   nodes <- cbind(nodes, terminfo_nodes)
#   
#   ##Get aliases and related terms for the nodes formatted
#   node.aliases <- lapply(nodelist, '[[', 'aliases')
#   alias_df <- unlist2df(node.aliases, names=c('node_id', 'ref_id'))
#   alias_df$type <- 'Alias'
#   node.related <- lapply(nodelist, '[[', 'relatedTerms')
#   related_df <- unlist2df(node.related, names=c('node_id', 'ref_id'))
#   related_df$type <- 'Related'
#   node.annot <- rbind(alias_df, related_df)
#   terminfo_annot <- terms[as.character(node.annot$ref_id), c('name', 'namespace', 'namespace_name')]
#   colnames(terminfo_annot)[1] <- 'ref'
#   node.annot <- cbind(node.annot, terminfo_annot)
#   
#   ##Retrieve all edges
#   edgelist <- nw$edges
#   edges <- jsonlist2df(edgelist)
#   ##Append information from the terms: namespace name and ID of predicate in this namespace
#   terminfo_edges <- terms[as.character(edges$p), c('name', 'namespace', 'namespace_name')]
#   colnames(terminfo_edges)[1] <- 'edge_desc'
#   edges <- cbind(edges, terminfo_edges)
#   
#   out <- new('ndexgraph',
#              nodes = nodes,
#              edges = edges,
#              node_annot = node.annot,
#              metadata = m,
#              name = m$network_name,
#              id = m$network_id)
# }

##########################################################
##Convert API exports to ndexgraph

#' Convert propertyGraph export to ndexgraph
#' 
#' @param property_graph Property graph data structure (as returned by relevant API functions)
#' @return \code{ndexgraph} object
#' @details Collects term ID, name and properties for nodes; 
#' predicate, subject, term ID and properties for edges. 
#' Each property takes up a column; 
#' column name is predicateString 
#' and value is \code{value} for given node/edge
#' @note PropertyGraphNetwork JSON data are more easily converted to ndexgraph objects than Network JSON data
#' @export
#' @examples
#' \dontrun{
#' x <- ndex.get.complete.network.as.property.graph('32733ca9-4e84-11e4-98fb-000c29873918')
#' y <- ndex.property.graph.as.ndexgraph(x)
#' }
ndex.property.graph.as.ndexgraph <- function(property_graph){
  
  ##Nodes
  nodes <- jsonlist2df(property_graph$nodes)
  ##Subset
  nodes <- nodes[,c('id', 'name')]
  
  ##Gather node properties
  if(length(property_graph$nodes[[1]]$properties) > 0){
    nodeprop_list <- lapply(property_graph$nodes, function(x){jsonlist2df(x$properties)})
    nodeprop <- sapply(nodeprop_list, `[[`, 'value')
    if(is.matrix(nodeprop)) nodeprop <- t(nodeprop)
    nodeprop <- as.data.frame(nodeprop, stringsAsFactors = FALSE)
    colnames(nodeprop) <- nodeprop_list[[1]]$predicateString
    
    nodes <- cbind(nodes, nodeprop)
  }
  
  ##Edges
  edges <- jsonlist2df(property_graph$edges)
  ##Subset
  edges <- edges[,c('subjectId', 'objectId', 'id', 'predicate')]
  
  ##Gather edge properties
  if(length(property_graph$edges[[1]]$properties) > 0){
    edgeprop_list <- lapply(property_graph$edges, function(x){jsonlist2df(x$properties)})
    edgeprop <- sapply(edgeprop_list, `[[`, 'value')
    if(is.matrix(edgeprop)) edgeprop <- t(edgeprop)
    edgeprop <- as.data.frame(edgeprop, stringsAsFactors = FALSE)
    colnames(edgeprop) <- edgeprop_list[[1]]$predicateString
    edges <- cbind(edges, edgeprop)
  }
  
  ##Properties
  prop <- jsonlist2df(property_graph$properties)
  
  out <- new('ndexgraph',
             nodes = nodes,
             edges = edges,
             properties = prop,
             name = property_graph$name,
             id = prop$value[prop$predicateString == 'NDEX:UUID'])
  return(out)
}


##########################################################
##Convert API exports to adjacency list (reannotation of nodes is main concern here)



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
  nd <- list.remove.nulls(nd)
  properties <- jsonlist2df(nd$properties)
  ##Grab network source (if any)
  source.idx <- which(properties$predicateString == 'Source')
  if(length(source.idx) == 1) ns <- properties$value[source.idx] else ns <- ''
  ##Grab network format (if any)
  format.idx <- which(properties$predicateString == 'Format')
  if(length(format.idx) == 1) nf <- properties$value[format.idx] else nf <- ''
  ##Grab species (if any)
  species.idx <- which(properties$predicateString == 'ORGANISM')
  if(length(species.idx) == 1) no <- properties$value[species.idx] else no <- ''
  ##Grab species (if any)
  uri.idx <- which(properties$predicateString == 'URI')
  if(length(uri.idx) == 1) uri <- properties$value[uri.idx] else uri <- ''
  
  out <- data.frame(network_id = nd$externalId,
                    network_name = nd$name,
                    node_count = nd$nodeCount,
                    edge_count = nd$edgeCount,
                    visibility = nd$visibility,
                    source = ns,
                    format = nf,
                    species = no,
                    uri = uri,
                    stringsAsFactors=FALSE)
  return(out)
}

list.remove.nulls <- function(x){
  if(!is.list(x)) stop("list expected")
  x <- lapply(x, function(x){
    if(is.null(x)) return('') 
    else return(x)
  })
  return(x)
}