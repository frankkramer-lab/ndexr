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
  if(class(propertyGraphNetwork) == 'list'){
    propertyGraphNetwork <- ndex.remove.uuid.from.network(propertyGraphNetwork)
    propertyGraphNetwork <- toJSON(propertyGraphNetwork)
  } else stop(sprintf("'list' expected, got '%s'", class(propertyGraphNetwork)))
  route <- "/network/asPropertyGraph"
  response <- ndex_rest_POST(route, propertyGraphNetwork)
  if(is.list(response)) out <- json_parse_network_metadata(response)
  else {
    warning("Posting network was unsuccessful")
    out <- NULL
  }
  return(out)
}

#   POST    /network/asPropertyGraph/group/{group UUID}    PropertyGraphNetwork    NetworkSummary
ndex.save.new.property.graph.network.for.group <- function(propertyGraphNetwork, groupId){
  if(class(propertyGraphNetwork) == 'list'){
    propertyGraphNetwork <- toJSON(propertyGraphNetwork)
  }
  route <- sprintf("/network/asPropertyGraph/group/%s", groupId)
  propertyGraphNetwork <- ndex.remove.uuid.from.network(propertyGraphNetwork)
  return(ndex_rest_POST(route, propertyGraphNetwork))
}

##  Neighborhood PathQuery
#   POST    /network/{networkUUID}/asPropertyGraph/query    SimplePathQuery    PropertyGraphNetwork 
ndex.get.neighborhood.as.property.graph <-function(networkId, searchString="", searchDepth=1){
  route <- sprintf("/network/%s/asPropertyGraph/query", networkId) 
  query <- toJSON(list(searchString = searchString, searchDepth = searchDepth))
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
  rownames(nodes) <- NULL
  
  ##Gather node properties
  nodeprop <- properties2df(property_graph$nodes)
  nodes <- cbind(nodes, nodeprop)
  
  ##Edges
  edges <- jsonlist2df(property_graph$edges)
  ##Subset
  edges <- edges[,c('subjectId', 'objectId', 'id', 'predicate')]
  rownames(edges) <- NULL
  
  ##Gather edge properties
  edgeprop <- properties2df(property_graph$edges)
  edges <- cbind(edges, edgeprop)
  
  ##Properties of the network
  prop <- jsonlist2df(property_graph$properties)
  
  name <- property_graph$name
  if(is.null(name)) name <- NA_character_
  
  uuid <- prop$value[prop$predicateString == 'NDEX:UUID']
  if(is.null(uuid)) uuid <- NA_character_
  out <- new('ndexgraph',
             nodes = nodes,
             edges = edges,
             properties = prop,
             name = name,
             id = uuid)
  return(out)
}


#' Convert ndexgraph to propertyGraph for import to NDEx
#' 
#' @param ndexgraph Object of class \code{\link{ndexgraph}}
#' @return List corresponding to propertyGraph JSON structure
#' @details Creates node elements from term ID, name; adds properties if there are any; 
#' Creates edge elements from predicate, subject, object ID, term ID and properties if there are any. 
#' @export
#' @examples
#' \dontrun{
#' x <- ndex.get.complete.network.as.property.graph('32733ca9-4e84-11e4-98fb-000c29873918')
#' y <- ndex.property.graph.as.ndexgraph(x)
#' z <- ndexgraph.as.ndex.property.graph(y)
#' }
ndexgraph.as.ndex.property.graph <- function(ndexgraph){
  if(class(ndexgraph) != 'ndexgraph') stop(sprintf("'ndexgraph' object expected, got '%s'", class(ndexgraph))) 
  
  ##Nodes
  nodes <- ndexgraph@nodes
  nodelist <- df2jsonlist(nodes[,1:2, drop = FALSE])
  node_prop <- df2properties(nodes[, -1:-2, drop = FALSE])
  for(i in seq_along(nodelist)){
    nodelist[[i]]$presentationProperties <- list()
    nodelist[[i]]$properties <- node_prop[[i]]
    nodelist[[i]]$type <- 'PropertyGraphNode'
  }
  
  ##Edges
  edges <- ndexgraph@edges
  edgelist <- df2jsonlist(edges[, 1:4, drop = FALSE])
  edge_prop <- df2properties(edges[, -1:-4, drop = FALSE])
  for(i in seq_along(edgelist)){
    edgelist[[i]]$presentationProperties <- list()
    edgelist[[i]]$properties <- edge_prop[[i]]
    edgelist[[i]]$type <- 'PropertyGraphEdge'
  }
  
  ##Properties of the network
  prop <- df2jsonlist(ndexgraph@properties)
  
  out <- list(edges = edgelist,
              nodes = nodelist,               
              presentationProperties = list(),
              name = ndexgraph@name, 
              properties = prop)
  return(out)
}


##########################################################
##Convert API exports to adjacency list (reannotation of nodes is main concern here)



##########################################################
##Auxiliary format conversion code (JSON -> list -> data frame)
#' Misc operations with lists (helps converting JSON to tabular formats)
#'
#' @param l list
#' @param df data frame
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
  if(length(l) == 0) return(as.data.frame(list()))
  ##Assemble rows
  ll <- lapply(l, jsonlist2vector)
  ##Check if all the vectors are of the same length (Actually might want to just align them all to the union of elements' names)
  ll_lengths <- sapply(ll, length)
  if(length(rle(ll_lengths)$values) > 1) stop("jsonlist2df: Elements of list have different structure, impossible to convert to a data frame")
  ##Merge rows into a table
  df <- do.call(rbind, ll)
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  return(df)
}

#' @rdname aux_list
#' 
#' @details \code{df2jsonlist} converts data frames to JSON-compatible lists of the same structure into a data frame. 
#' Each row of data frame corresponds to first order element in list
#'  for each row, missing values are replaced with nulls; non-vector elements and vector elements with length > 1 are dropped
df2jsonlist <- function(df){
  if(!is.data.frame(df)) stop(sprintf("'data.frame' expected, got '%s'", class(df)))
  if(nrow(df) == 0) return(list())
  ##COnvert to list of lists
  ll <- apply(df, 1, as.list)
  
  return(ll)
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


#' Convert properties of nodes / edges to a data frame
#' @param entities List of node or edge elements from 'propertyGraph'
#' @param missing.value what to insert when a property is missing for a given element; default NA
#' @return Data frame with all properties; missing values defined by missing.value
#' @details Each node or edge can have properties (possibly not the same number of values across nodes/edges)
#' Our goal is to turn them to the data frame, covering all properties and putting NA for the missing properties if those happen.
#' @examples
#' \dontrun{
#' }
properties2df <- function(entities, missing.value = NA){
  ##Check if properties exist at all
  property_count <- sapply(entities, function(x){length(x$properties)})
  if(max(property_count) == 0){
    ##Special case of no properties (does it happen actually?)
    ##Return an empty DF
    df <- data.frame(row.names = names(entities), stringsAsFactors = FALSE)
    return(df)
  }
  
  ##Otherwise, collect the properties
  prop_list <- lapply(entities, function(x){jsonlist2df(x$properties)})
  ##This is a list of data frames
  property_names_list <- lapply(prop_list, `[[`, 'predicateString')
  property_names <- Reduce(union, property_names_list)
  ##Now we have to bring all data frames to the common row order
  prop_list_ordered <- lapply(prop_list, function(x){x[match(property_names, x$predicateString), , drop = FALSE]})
  ##Now, construct the matrix of all property  values 
  ##At this moment, property value vectors must be of same length and same order
  prop_matrix <- sapply(prop_list_ordered, `[[`, 'value')
  ##Fill in the NAs with desired missing values
  if(!is.na(missing.value)) prop_matrix[is.na(prop_matrix)] <- missing.value[1L]
  if(is.matrix(prop_matrix)) prop_matrix <- t(prop_matrix)
  ##Bring to DF and return
  prop_matrix <- as.data.frame(prop_matrix, stringsAsFactors = FALSE)
  colnames(prop_matrix) <- property_names
  return(prop_matrix)
  
}

#' Convert data frame of properties back to the list structure eligible to be used with propertyGraph
#' @param df data frame made of node or edge properties
#' @return properties in the list form
#' @details This is needed to save the networks back to NDEx
df2properties <- function(df, missing.value = NA){
  if(!is.na(missing.value)) df[df == missing.value] <- NA
  ##Form list for an edge
  get_properties <- function(x){
    names <- names(x)
    names(x) <- NULL
    out <- vector(mode = 'list', length = length(x))
    for(i in seq_along(x)){
      if(is.na(x[i])) next
      out[[i]] <- list(dataType = 'String',
                       predicateString = names[i],
                       predicateId = 0,
                       valueId = 0,
                       value = x[i],
                       type = 'NdexPropertyValuePair')
    }
    out <- out[!sapply(out, is.null)]
    return(out)
  }
  
  prop_values <- apply(df, 1, get_properties)
  return(prop_values)
}

#' Auxiliary function: remove UUID from a network
#' @param network network (as propertyGraph, so far)
#' @return network with UUID replaced with NULL
ndex.remove.uuid.from.network <- function(network){
  idx <- which(sapply(network$properties, `[[`, 'predicateString') == 'NDEX:UUID')
  if(length(idx) == 1){
    ##The R usually drops an element from list when assigning NULL to it, so we do a little trick here
    network$properties[[idx]]['value'] <- list(NULL)
  }
  return(network)
}
