##Authors:
#   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##Created: 6 June 2014
# Contains functions to connect with CBDD

#' Get basic network eligible for CBDD from \code{\link{ndexgraph}} object
#' 
#' @param g \code{\link{ndexgraph}} object
#' @param useNamespace (optional) name of the namespace for nodes to use
#' @return data frame with two columns containing IDs of interacting nodes
ndexgraph2cbdd <- function(g, useNamespace){
  if(class(g) != 'ndexgraph') stop("ndexgraph object expected")
  
  edges <- g@edges[,c('s', 'o')]
  nodes <- g@nodes
  
  ##Reencode the numeric IDs with the default term IDs for nodes
  #This may not make sense for complicated networks which contain tons of IDs from disparate namespaces
  edges_annot <- data.frame(node1 = nodes$ref[match(edges$s, nodes$node_id)],
                            node2 = nodes$ref[match(edges$o, nodes$node_id)],
                            stringsAsFactors = FALSE)
  
  if(!missing(useNamespace)){
    ##Check if this namespace exists
    g_n <- unique(c(nodes$namespace_name, g@node_annot$namespace_name))
    if(!useNamespace %in% g_n) stop("No such namespace")
  }
  
}