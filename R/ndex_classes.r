##Author: Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##Created: 6 June 2014
# Contains class definitions for the graph objects

setOldClass('igraph') ##S3 class

##########################################
##Generic network based on data frames with node and edge properties
##########################################

##Additional slots? Owning user, publicness, namespaces?
setClass("ndexgraph",
         representation(nodes = "data.frame", edges = "data.frame", name = "character", id = "numeric"),
         prototype = list(nodes = data.frame(id=character(), name=character()),
                          edges = data.frame(id1=character(), id2=character(), mechanism=numeric(), effect=numeric()),
                          name = NA_character_,
                          id = NA_real_
         ),
         validity = validate_ndexgraph)


##validator
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


