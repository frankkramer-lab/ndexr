##Authors:
#   Frank Kramer [frank.kramer@med.uni-goettingen.de]
## Created: 20 Sep 2016
## Base functions to create, parse, modify ngraph objects from/to RCX data


#' Create ngraph object from RCX object
#' 
#' @param rcx RCX object
#' @param verbose logical; whether to print out extended feedback 
#' @return returns object of class ngraph if successfull, NULL otherwise
#' @export
ndex.RCX2ngraph <- function(rcx, verbose = FALSE){
  
  if(!("RCX" %in% class(rcx))) {
    warning("RCX2ngraph: supplied parameter is not of class RCX! Returning null.")
    return(NULL)
  }

  ##### create empty graph
  ngraph = igraph::make_empty_graph()
  # adding graph attributes is harmless
  ngraph = ndex.internal_addAspects(ngraph, rcx, verbose)
  
  ## sanity checks: no nodes defined
  if(is.null(rcx$nodes) || dim(rcx$nodes)[1] == 0) {
    warning("RCX2ngraph: supplied RCX does not contain node information. Returning ngraph object without nodes or edges.")
    return(ngraph)
  }

  ## add nodes and nodeAttributes (if available)    
  ngraph = igraph::add_vertices(ngraph, length(rcx$nodes$"@id"),name=rcx$nodes$"@id",attr=as.list(rcx$nodes))
  if(!is.null(rcx$nodeAttributes) && dim(rcx$nodeAttributes)[1] > 0) {
    for(attrname in unique(rcx$nodeAttributes$n)) {
      sel = rcx$nodeAttributes$n == attrname
      ngraph = igraph::set_vertex_attr(ngraph,attrname, index=as.character(rcx$nodeAttributes$po[sel]), value=as.list(rcx$nodeAttributes$v[sel]))
    }
  }
  
  ## sanity checks: no edges defined
  if(is.null(rcx$edges) || dim(rcx$edges)[1] == 0) {
    warning("RCX2ngraph: supplied RCX does not contain edge information. Returning ngraph object without edges.")
    return(ngraph)
  }
  
  #not used: ngraph = igraph::graph_from_data_frame(d = rcx$edges[,c("s","t","@id","i")], vertices = cbind(rcx$nodes$"@id",rcx$nodes))
  # add edges and edgeAttributes (if available) 
  ngraph = igraph::add_edges(ngraph,edges=as.character(c(t(rcx$edges[,c("s","t")]))),attr=rcx$edges[,c("@id","i")])
  if(!is.null(rcx$edgeAttributes) && dim(rcx$edgeAttributes)[1] > 0) {
    for(attrname in unique(rcx$edgeAttributes$n)) {
      sel = rcx$edgeAttributes$n == attrname
      ngraph = igraph::set_edge_attr(ngraph, attrname, index=match(rcx$edgeAttributes$po[sel],E(ngraph)$"@id"), value=as.list(rcx$edgeAttributes$v[sel]))
    }
  }
  
  class(ngraph) = c("ngraph",class(ngraph))
  return(ngraph)
  
}


#' ndex.internal_addAspects
#' 
#' @param ngraph ngraph object
#' @param rcx RCX object
#' @param verbose logical; whether to print out extended feedback 
#' @return returns object of class ngraph
ndex.internal_addAspects <- function(ngraph, rcx, verbose = FALSE){
  
  ### add all non-core aspects to the graph:
  for(i in names(rcx)) {
    if(i %in% c("nodes","edges","nodeAttributes","edgeAttributes")) {
      next()
    }
    ngraph = igraph::set_graph_attr(ngraph,i,rcx[[i]])
  }
  
  # # if networkAttributes available
  # if(is.null(rcx$networkAttributes) || dim(rcx$networkAttributes)[1] == 0) return(ngraph)
  # ### unlist the network attributes
  # for(i in 1:length(rcx$networkAttributes$n)) {
  #   ngraph = igraph::set_graph_attr(ngraph,name=rcx$networkAttributes$n[i],value=rcx$networkAttributes$v[i])
  # }
  
  return(ngraph)
}


#' Create RCX object from ngraph object
#' 
#' @param ngraph ngraph object
#' @param verbose logical; whether to print out extended feedback 
#' @return returns object of class RCX if successfull, NULL otherwise
#' @export
ndex.ngraph2RCX <- function(ngraph, verbose = FALSE){
  
  if(is.null(ngraph) || !("igraph" %in% class(ngraph))) {
    warning("ndex.ngraph2RCX: parameter ngraph does not contain igraph object")
    return(NULL)
  }
  
  aspectlist = list()
  #set class
  class(aspectlist) = c("RCX",class(aspectlist))
  
  sel = igraph::list.graph.attributes(ngraph)
  for(i in sel) {
    aspectlist[[i]] = igraph::get.graph.attribute(ngraph,i)
  }
  
  ## pick apart nodes/edges and their attributes
  tmp = igraph::as_data_frame(ngraph,what="both")
  
  if(!is.null(tmp$vertices) && dim(tmp$vertices)[1] > 0) {
    
    #nodes
    tmp2 = as.data.frame(tmp$vertices[,"@id"], stringsAsFactors=F, row.names = NULL)
    colnames(tmp2) = c("@id")
    if("n" %in% colnames(tmp$vertices)) {
      tmp2$n = tmp$vertices$n
    } 
    if ("r" %in% colnames(tmp$vertices)) {
      tmp2$r = tmp$vertices$r
    }
    aspectlist[["nodes"]] = tmp2
    
    #nodeAttributes
    sel = !(colnames(tmp$vertices) %in% c("name","@id","n","r"))
    if(any(sel)) {
      tmp2 = as.data.frame(tmp$vertices[,"@id"], stringsAsFactors=F, row.names = NULL)
      colnames(tmp2) = c("po")
      tmp2 = as.data.frame(cbind(tmp2, tmp$vertices[,sel]), stringsAsFactors=F)
      row.names(tmp2) = NULL
      tmp2 = tidyr::gather_(tmp2,"n","v",colnames(tmp$vertices)[sel])
      tmp2 = plyr::arrange(tmp2,po)
      tmp2$d = "list_of_string"
      aspectlist[["nodeAttributes"]] = tmp2
    }
  }
  
  
  if(!is.null(tmp$edges) && dim(tmp$edges)[1] > 0) {
    #edges
    tmp2 = as.data.frame(cbind(tmp$edges[,"@id"],tmp$edges[,"from"],tmp$edges[,"to"]), stringsAsFactors=F, row.names = NULL)
    colnames(tmp2) = c("@id","s","t")
    if("i" %in% colnames(tmp$edges)) {
      tmp2$i = tmp$edges$i
    }
    aspectlist[["edges"]] = tmp2
    
    #edgeAttributes
    sel = !(colnames(tmp$edges) %in% c("@id","from","to","i"))
    if(any(sel)) {
      tmp2 = as.data.frame(tmp$edges[,"@id"], stringsAsFactors=F, row.names = NULL)
      colnames(tmp2) = c("po")
      tmp2 = as.data.frame(cbind(tmp2, tmp$edges[,sel]), stringsAsFactors=F)
      row.names(tmp2) = NULL
      tmp2 = tidyr::gather_(tmp2,"n","v",colnames(tmp$edges)[sel])
      tmp2 = plyr::arrange(tmp2,po)
      tmp2$d = "list_of_string"
      aspectlist[["edgeAttributes"]] = tmp2
    }
  }

  return(aspectlist)
  
}

