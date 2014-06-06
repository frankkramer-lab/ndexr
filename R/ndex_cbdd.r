##Authors:
#   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##Created: 6 June 2014
# Contains functions to connect with CBDD

#' Get basic network eligible for CBDD from \code{\link{ndexgraph}} object
#' 
#' @param g \code{\link{ndexgraph}} object
#' @param useNamespace (optional) name of the namespace for nodes
#' @return data frame with two columns containing IDs of interacting nodes
#' @details
#' By default, node IDs will be the references from the basic terms ('represents' component of Node)
#' If useNamespace is supplied, function will try to reencode nodes with given namespace using aliases and related terms stored in \code{node_annot} slot of input
#' @export
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
    ##Get all terms within this namespace
    all_node_annot <- subset(g@node_annot, namespace_name == useNamespace)
    idx_to_reannot <- !nodes$namespace_name %in% useNamespace
    nodes_to_reannot <- nodes$node_id[idx_to_reannot]
    ##Get all nodes which can in principle be reannotated
    nodes_to_update <- intersect(nodes_to_reannot, all_node_annot$node_id)
    cat(length(nodes_to_update), "nodes of", length(nodes_to_reannot), 
        "can be reannotated in", useNamespace, "terms\n")
    
    cat(length(nodes_to_reannot) - length(nodes_to_update), "nodes lack", useNamespace, "terms\n")
    new_annot <- subset(all_node_annot, node_id %in% nodes_to_update)
    
    ##Get positions of these nodes in edges
    efrom.idx <- match(edges$s, new_annot$node_id)
    
    eto.idx <- match(edges$o, new_annot$node_id)
    ##Replace node IDs in the found positions with refs from new_annot
    edges_annot[!is.na(efrom.idx), 1] <- new_annot$ref[efrom.idx[!is.na(efrom.idx)]]
    edges_annot[!is.na(eto.idx), 2] <- new_annot$ref[eto.idx[!is.na(eto.idx)]]
    
  }
  
  return(edges_annot)
}