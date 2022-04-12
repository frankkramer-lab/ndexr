################################################################################
## Authors:
##   Frank Kramer [frank.kramer@informatik.uni-augsburg.de]
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
##   Zaynab Hammoud [zaynab.hammoud@informatik.uni-augsburg.de]
##
## History:
##   Created on 20 September 2016 by Kramer
##   Copied from NDExR package on 3 August 2017 by Auer
##
## Description:
##    Base functions to create, parse, modify RCXgraph/igraph objects from/to CX networks
################################################################################


#############################################################
## 
##   Funtions for conversion RCX ==> RCXgraph
##
#############################################################


#' @title Create RCXgraph object from RCX object 
#' 
#' \strong{Note: In future `ndexr` uses the \link[RCX]{RCX-object} from the corresponding package to handle the networks!}
#' \strong{For a replacement of this function see \link[RCX]{toIgraph} and \link[RCX]{fromIgraph}!}
#'
#' @param rcx \code{\link{RCX}} object
#' @param idAsVertexName logical; whether the ndex node id ("@id") should be used as name for the igraph node (i.e. vertex). By default the "n"ame property is used
#' @param idAsEdgeName logical; whether the ndex edge id ("@id") should be used as name for the igraph edge. By default the "i"nteraction property is used
#' @param verbose logical; whether to print out extended feedback
#'
#' @return returns object of class RCXgraph if successfull, NULL otherwise
#'
#' @template RCXconversionRCXgraph
#' @template RCXgraph
#'
#' @seealso \code{\link{rcxgraph_toRCX}} \code{\link{rcx_fromRCXgraph}} \code{\link{rcx_fromJSON}} \code{\link{rcx_toJSON}} \code{\link{RCX}} \code{\link[igraph]{igraph}}
#' @aliases RCXgraph
#' @example man-roxygen-examples/RCXconversionRCXgraph.R
#' @export rcx_toRCXgraph rcxgraph_fromRCX
rcx_toRCXgraph <- function(rcx, idAsVertexName = FALSE, idAsEdgeName = FALSE, verbose = FALSE){
  .Deprecated("RCX::toIgraph()")

  if(!("RCX" %in% class(rcx))) {
    warning("RCX2RCXgraph: supplied parameter is not of class RCX! Returning null.")
    return(NULL)
  }

  ##### create empty graph
  rcxgraph = igraph::make_empty_graph()
  
  # adding all non core aspects
  rcxgraph = ndex_internal_addAspects(rcxgraph, rcx, verbose)

  ## sanity checks: no nodes defined
  if(is.null(rcx$nodes) || dim(rcx$nodes)[1] == 0) {
    warning("RCX2RCXgraph: supplied RCX does not contain node information. Returning RCXgraph object without nodes or edges.")
    return(rcxgraph)
  }

  ## add nodes and nodeAttributes (if available)
  ## add nodes
  ids = as.character(rcx$nodes$"@id")
  #nodes = as.list(rcx$nodes[names(rcx$nodes)!='@id'])
  nodes = as.list(rcx$nodes)
  rcxgraph = igraph::add_vertices(rcxgraph, length(ids), name=ids, attr=nodes)
  
  ## add node attributes
  if(!is.null(rcx$nodeAttributes) && dim(rcx$nodeAttributes)[1] > 0) {
    for(attrname in unique(rcx$nodeAttributes$n)) {
      sel = rcx$nodeAttributes$n == attrname
      ## in igraph the attribute 'name' is used for the id (here @id) of the node, therefore the attribute name has to be changed!
      if(attrname=='name') attrname='NAME'
      ## !ToDo: value type is missing: e.g. n=selected, v=false, d=boolean
      ## Therefore the encoding of list_of... might be incorrect!
      if(verbose){ message('___________\nRCXgraph:\n\tattrname:',attrname, '\n\tindex: ',index=paste(as.character(rcx$nodeAttributes$po[sel]),collapse = ', '),'\n\tvalue: ', value=paste(rcx$nodeAttributes$v[sel],collapse = ', '),'\n') }
      
      rcxgraph = igraph::set_vertex_attr(rcxgraph,
                                         attrname, 
                                         index=as.character(rcx$nodeAttributes$po[sel]), 
                                         value=rcx$nodeAttributes$v[sel])
    }
  }

  ## sanity checks: no edges defined
  if(is.null(rcx$edges) || dim(rcx$edges)[1] == 0) {
    warning("RCX2RCXgraph: supplied RCX does not contain edge information. Returning RCXgraph object without edges.")
    return(rcxgraph)
  }

  ## add edges and edgeAttributes (if available)
  ## add edges
  edges = as.character(c(t(rcx$edges[,c("s","t")])))
  edgeNames = as.character(rcx$edges$'@id')
  edgeAttr = rcx$edges
  rownames(edgeAttr) = edgeAttr$'@id'
  edgeAttr$s = NULL
  edgeAttr$t = NULL
  #edgeAttr$'@id' = NULL
  #rcxgraph = igraph::add_edges(rcxgraph,edges=c(t(rcx$edges[,c("s","t")])),attr=edgeAttr)
  #rcxgraph = igraph::add_edges(rcxgraph,edges=c(t(rcx$edges[,c("s","t")])),attr=rcx$edges[,c("@id","i")])
  #rcxgraph = igraph::add_edges(rcxgraph,edges=c(t(rcx$edges[,c("s","t")])))

  rcxgraph = igraph::add_edges(rcxgraph, edges=edges, name=edgeNames, attr=edgeAttr)
  
  ## add edge attrubutes
  if(!is.null(rcx$edgeAttributes) && dim(rcx$edgeAttributes)[1] > 0) {
    for(attrname in unique(rcx$edgeAttributes$n)) {
      sel = rcx$edgeAttributes$n == attrname
      ## in igraph the attribute 'name' is used for the id (here @id) of the node, therefore the attribute name has to be changed!
      if(attrname=='name') attrname='NAME'
      if(verbose){ message('___________\nRCXgraph:\n\tattrname:',attrname, '\n\tindex: ',index=paste(as.character(rcx$edgeAttributes$po[sel]),collapse = ', '),'\n\tvalue: ', value=paste(rcx$edgeAttributes$v[sel],collapse = ', '),'\n') }
      rcxgraph = igraph::set_edge_attr(rcxgraph, attrname, index=as.character(rcx$edgeAttributes$po[sel]), value=rcx$edgeAttributes$v[sel])
    }
  }
  
  ## @id as name was necessary for linking aspects together
  ## now change the name to node name/edge interaction
  if(!idAsVertexName) V(rcxgraph)$name = V(rcxgraph)$n
  if(!idAsEdgeName) E(rcxgraph)$name = E(rcxgraph)$i
  
  ## make it a RCXgraph object
  class(rcxgraph) = c("RCXgraph",class(rcxgraph))
  return(rcxgraph)
}

#' @rdname rcx_toRCXgraph
rcxgraph_fromRCX <- rcx_toRCXgraph


#############################################################
## 
##   Internal Helper funtion for conversion RCX ==> RCXgraph
##
#############################################################


#' ndex_internal_addAspects
#'
#' @param rcxgraph RCXgraph object
#' @param rcx RCX object
#' @param verbose logical; whether to print out extended feedback
#' @return returns object of class RCXgraph
#' 
#' @keywords internal
#' @examples
#' NULL
ndex_internal_addAspects <- function(rcxgraph, rcx, verbose = FALSE){

  ### add all non-core aspects to the graph:
  for(i in names(rcx)) {
    if(i %in% c("nodes","edges","nodeAttributes","edgeAttributes")) {
      next()
    }
    rcxgraph = igraph::set_graph_attr(rcxgraph,i,rcx[[i]])
  }

  # # if networkAttributes available
  # if(is.null(rcx$networkAttributes) || dim(rcx$networkAttributes)[1] == 0) return(rcxgraph)
  # ### unlist the network attributes
  # for(i in 1:length(rcx$networkAttributes$n)) {
  #   rcxgraph = igraph::set_graph_attr(rcxgraph,name=rcx$networkAttributes$n[i],value=rcx$networkAttributes$v[i])
  # }

  return(rcxgraph)
}


#############################################################
## 
##   Deprecated funtions for conversion RCX ==> NGraph
##
#############################################################

#' Deprecated: Create NGraph object from RCX object
#' 
#' @param rcx \code{\link{RCX}} object
#' @param verbose logical; whether to print out extended feedback 
#' 
#' @return returns object of class ngraph if successfull, NULL otherwise
#' 
#' @description These functions are provided for compatibility with older versions of \sQuote{ndexr} only, and will be defunct at the next release.
#' @details The following functions are deprecated and will be made defunct; use the replacement indicated below:
#'  \itemize{
#'  \item{rcx_toNGraph: \code{\link{rcx_toRCXgraph}}}
#'  \item{ngraph_fromRCX: \code{\link{rcxgraph_fromRCX}}}
#'  }
#' 
#' @templateVar graphObject NGraph
#' @template RCXconversionRCXgraph
#' @template RCXgraph
#' 
#' @seealso \code{\link{rcx_toRCXgraph}} \code{\link{rcxgraph_fromRCX}} \code{\link{rcxgraph_toRCX}} \code{\link{rcx_fromRCXgraph}} \code{\link{rcx_fromJSON}} \code{\link{rcx_toJSON}} \code{\link{RCX}} \code{\link[igraph]{igraph}}
#' @aliases ngraph
#' @keywords internal
#' @example man-roxygen-examples/RCXconversionNGraph.R 
#' @export ngraph_fromRCX rcx_toNGraph
rcx_toNGraph <- function(rcx, verbose = FALSE) {
    .Deprecated("rcx_toRCXgraph")
    rcx_toRCXgraph(rcx, verbose)
}

#' @rdname rcx_toNGraph
ngraph_fromRCX <- function(rcx, verbose = FALSE) {
    .Deprecated("rcxgraph_fromRCX")
    rcx_toRCXgraph(rcx, verbose)
}


#############################################################
## 
##   Funtions for conversion RCXgraph ==> RCX
##
#############################################################


#' @title Create RCX object from RCXgraph object
#' 
#' \strong{Note: In future `ndexr` uses the \link[RCX]{RCX-object} from the corresponding package to handle the networks!}
#' \strong{For a replacement of this function see \link[RCX]{toIgraph} and \link[RCX]{fromIgraph}!}
#'
#' @param rcxgraph \code{\link{RCX}} object
#' @param verbose logical; whether to print out extended feedback
#'
#' @return returns object of class \code{\link{RCX}} if successfull, NULL otherwise
#'
#' @template RCXgraphConversionRCX
#' @template RCXgraph
#'
#' @seealso \code{\link{rcxgraph_fromRCX}} \code{\link{rcx_toRCXgraph}} \code{\link{rcx_fromJSON}} \code{\link{rcx_toJSON}} \code{\link{RCX}} \code{\link[igraph]{igraph}}
#' 
#' @example man-roxygen-examples/RCXconversionRCXgraph.R
#' @export rcxgraph_toRCX rcx_fromRCXgraph
rcxgraph_toRCX <- function(rcxgraph, verbose = FALSE){
  .Deprecated("RCX::fromIgraph()")

  if(is.null(rcxgraph) || !("igraph" %in% class(rcxgraph))) {
    warning("rcxgraph_toRCX: parameter rcxgraph does not contain igraph object")
    return(NULL)
  }

  aspectlist = list()
  #set class
  class(aspectlist) = c("RCX",class(aspectlist))

  sel = igraph::list.graph.attributes(rcxgraph)
  for(i in sel) {
    aspectlist[[i]] = igraph::get.graph.attribute(rcxgraph,i)
  }

  ## pick apart nodes/edges and their attributes
  tmp = igraph::as_data_frame(rcxgraph,what="both")

  if(!is.null(tmp$vertices) && dim(tmp$vertices)[1] > 0) {

    #nodes
    tmp2 = as.data.frame(tmp$vertices[,"@id"], stringsAsFactors=FALSE, row.names = NULL)
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
      tmp2 = as.data.frame(tmp$vertices[,"name"], stringsAsFactors=FALSE, row.names = NULL)
      colnames(tmp2) = c("po")
      tmp2 = as.data.frame(cbind(tmp2, tmp$vertices[,sel,drop=FALSE]), stringsAsFactors=FALSE)
      row.names(tmp2) = NULL
      tmp2 = tidyr::gather_(tmp2,"n","v",colnames(tmp$vertices)[sel])
      tmp2 = plyr::arrange(tmp2,po)
      tmp2$d = "list_of_string" ## !TODO: wrong!
      ## correct the nodeAttributes property NAME to lower case in RCX
      colnames(tmp2)[which(colnames(tmp2) == "NAME")] = 'name'
      aspectlist[["nodeAttributes"]] = tmp2
    }
  }


  if(!is.null(tmp$edges) && dim(tmp$edges)[1] > 0) {
    #edges
    tmp2 = as.data.frame(cbind(tmp$edges[,"@id"],tmp$edges[,"from"],tmp$edges[,"to"]), stringsAsFactors=FALSE, row.names = NULL)
    colnames(tmp2) = c("@id","s","t")
    if("i" %in% colnames(tmp$edges)) {
      tmp2$i = tmp$edges$i
    }
    aspectlist[["edges"]] = tmp2

    #edgeAttributes
    sel = !(colnames(tmp$edges) %in% c("@id","from","to","i"))
    if(any(sel)) {
      tmp2 = as.data.frame(tmp$edges[,"@id"], stringsAsFactors=FALSE, row.names = NULL)
      colnames(tmp2) = c("po")
      tmp2 = as.data.frame(cbind(tmp2, tmp$edges[,sel,drop=FALSE]), stringsAsFactors=FALSE)
      row.names(tmp2) = NULL
      tmp2 = tidyr::gather_(tmp2,"n","v",colnames(tmp$edges)[sel])
      tmp2 = plyr::arrange(tmp2,po)
      tmp2$d = "list_of_string"
      aspectlist[["edgeAttributes"]] = tmp2
    }
  }

  return(aspectlist)

}

#' @rdname rcxgraph_toRCX
rcx_fromRCXgraph <- rcxgraph_toRCX


#############################################################
## 
##   Deprecated funtions for conversion RCXgraph ==> RCX
##
#############################################################


#' @title Deprecated: Create RCX object from NGraph object
#'
#' @param ngraph NGraph object
#' @param verbose logical; whether to print out extended feedback
#'
#' @return returns object of class \code{\link{RCX}} if successfull, NULL otherwise
#'
#' @description These functions are provided for compatibility with older versions of \sQuote{ndexr} only, and will be defunct at the next release.
#' @details The following functions are deprecated and will be made defunct; use the replacement indicated below:
#'  \itemize{
#'  \item{rcx_toNGraph: \code{\link{rcx_toRCXgraph}}}
#'  \item{ngraph_fromRCX: \code{\link{rcxgraph_fromRCX}}}
#'  }
#' 
#' @templateVar graphObject NGraph
#' @template RCXgraphConversionRCX
#' @template RCXgraph
#'
#' @seealso \code{\link{rcxgraph_fromRCX}} \code{\link{rcx_toRCXgraph}} \code{\link{rcx_fromJSON}} \code{\link{rcx_toJSON}} \code{\link{RCX}} \code{\link[igraph]{igraph}}
#' @keywords internal
#' @example man-roxygen-examples/RCXconversionNGraph.R
#' @export
ngraph_toRCX <- function(ngraph, verbose = FALSE) {
    .Deprecated("rcxgraph_toRCX")
    rcxgraph_toRCX(ngraph, verbose)
}

#' @rdname ngraph_toRCX
rcx_fromNGraph <- function(ngraph, verbose = FALSE) {
    .Deprecated("rcx_fromRCXgraph")
    rcxgraph_toRCX(ngraph, verbose)
}


