##Authors:
#   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##Created: 6 June 2014
# Contains class definitions for the graph objects

setOldClass('igraph') ##S3 class

##########################################
##Generic network based on data frames with node and edge properties
##########################################

#' Validator for ndexgraph class
#' @param object R object to be tested
#' @return returns TRUE if the object is valid instance of \code{ndexgraph} S4 class; otherwise returns vector with error messages
#' @note Additional validations may be needed!
validate_ndexgraph <- function(object){
  nodes <- object@nodes
  edges <- object@edges
    
  errors <- character()
  ##Check presence of necessary attributes
  if(ncol(edges) < 1){
    errors <- c(errors, "Node data frame should contain at least 1 column")
  }
  if(ncol(edges) < 2){
    errors <- c(errors, "Edge data frame should contain at least 2 columns (interacting node IDs)")
  }

  ##Finalize check
  if(length(errors) == 0){
    return(TRUE)
  }else{
    return(errors)
  }
}

##Class definition
##Additional slots? Owning user, publicness, namespaces?

#' S4 class representing NDEx network
#' 
#' @slot nodes Data frame with node attributes
#' @slot edges Data frame with edge attributes
#' @slot node_annot Data frame with additional information on nodes (node aliases or IDs from other namespaces)
#' @slot metadata Data frame containing general information about the network
#' @slot name Name of the network
#' @slot id Unique ID of the network
#' @note So far ID and name are optional (by default will be NA upon initialization, and go unchecked by validator).
#' @export
setClass("ndexgraph",
         representation(nodes = "data.frame", edges = "data.frame",
                        node_annot = "data.frame", metadata = "data.frame",
                        name = "character", id = "character"),
         prototype = list(nodes = data.frame(node_id=character()),
                          edges = data.frame(node_id1=character(), node_id2=character()),
                          node_annot = data.frame(node_id=character()),
                          metadata = data.frame(id=character()),
                          name = NA_character_,
                          id = NA_character_
         ),
         validity = validate_ndexgraph)