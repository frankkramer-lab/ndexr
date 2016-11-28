##Authors:
#   Frank Kramer [frank.kramer@med.uni-goettingen.de]
## Created: 20 Sep 2016
## Base functions to create, parse, modify ngraph objects from/to RCX data


#' Create ngraph object from RCX object
#' 
#' This function creates an ngraph object from a supplied \code{\link{RCX}} object.
#' RCX objects store the CX data as a named list of data.frames containing metaData and all aspects of the network.
#' The ngraph class inherits from igraph and contains the complete (R)CX information as graph, node and edge attributes.
#' All \code{\link[igraph]{igraph}} functionality is available, e.g. access nodes and edges of igraph g via V(g) and E(g) and their attributes via V(g)$attribute
#' 
#' The following rules apply to convert from \code{\link{RCX}} to ngraph:
#' \itemize{
#'  \item nodes receive the "@id" value as name. All other information in aspects node and nodeAttributes are saved as node attributes, access via V(g). 
#'        Data goes from long format (column n containing attribute name and column v containing attribute value) to wide format (columns for each unique n with cells contianing v).
#'  \item edges are connected via their "s"art and "t"arget fields. The "@id" and "i"nteraction attribute are stored as is and all edgeAttributes are saved as node attributes, access via E(g). 
#'        Data goes from long format (column n containing attribute name and column v containing attribute value) to wide format (columns for each unique n with cells contianing v).
#'  \item all other aspect data is stored as graph attributes, access via g$aspect
#' }
#' 
#' An ngraph object could look like this:\cr
#'\preformatted{
#'> str(ngraph)
#'  IGRAPH DN-- 5 11 -- PLK3 signaling events
#'  + attr: name (g/c), description (g/c), version (g/c), ndex:sourceFormat (g/c), name (v/c), @id (v/n), n
#'  | (v/c), test (v/c), relatedTo (v/x), @id (e/n), i (e/c)
#'  + edges (vertex names):
#'   [1] 60714376->60714377 60714381->60714377 60714384->60714377 60714377->60714376 60714377->60714381 60714377->60714381
#'   [7] 60714377->60714384 60714377->60714384 60714377->60714395 60714377->60714395 60714377->60714395
#'> V(ngraph)
#'  + 5/5 vertices, named:
#'   [1] 60714376 60714377 60714381 60714384 60714395
#'> V(ngraph)$n
#'  [1] "CCNE1" "PLK3"  "MPIP3" "CHK2"  "P53"
#'> E(ngraph)
#'  + 11/11 edges (vertex names):
#'   [1] 60714376->60714377 60714381->60714377 60714384->60714377 60714377->60714376 60714377->60714381 60714377->60714381
#'   [7] 60714377->60714384 60714377->60714384 60714377->60714395 60714377->60714395 60714377->60714395
#'> E(ngraph)$i
#'  [1] "neighbor-of"                 "neighbor-of"                 "neighbor-of"                
#'  [4] "controls-expression-of"      "controls-phosphorylation-of" "controls-state-change-of"   
#'  [7] "controls-phosphorylation-of" "controls-state-change-of"    "controls-phosphorylation-of"
#'  [10] "controls-state-change-of"    "neighbor-of"                
#'}
#' 
#' 
#' @param rcx RCX object
#' @param verbose logical; whether to print out extended feedback 
#' @return returns object of class ngraph if successfull, NULL otherwise
#' @seealso \code{\link{ndex.ngraph2RCX}} \code{\link{ndex.JSON2RCX}} \code{\link{ndex.RCX2JSON}} \code{\link{RCX}} \code{\link[igraph]{igraph}}   
#' @aliases ngraph
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.complete.network(ndexcon,pws[1,"externalId"]) 
#' rcx$edges
#' ngraph = ndex.RCX2ngraph(rcx) 
#' ngraph
#' E(ngraph) }
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
      if(verbose){ cat('___________\nngraph:\n\tattrname:',attrname, '\n\tindex:',index=as.character(rcx$nodeAttributes$po[sel]),'\n\tvalue:', value=rcx$nodeAttributes$v[sel],'\n') }
      ngraph = igraph::set_vertex_attr(ngraph,attrname, index=rcx$nodeAttributes$po[sel], value=as.list(rcx$nodeAttributes$v[sel]))
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
      ngraph = igraph::set_edge_attr(ngraph, attrname, index=match(rcx$edgeAttributes$po[sel],igraph::E(ngraph)$"@id"), value=as.list(rcx$edgeAttributes$v[sel]))
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
#' This function creates an RCX object from a valid ngraph object.
#' 
#' The following rules apply to convert from \code{\link{ngraph}} to \code{\link{RCX}}:
#' \itemize{
#'  \item all graph attributes are stored as named data.frames within the RCX object
#'  \item nodes receive their name value as "@id" attribute. All other node attributes are saved in the RCX object as nodeAttributes, access via rcx[["nodeAttributes"]]. 
#'        Data goes from wide format (columns for each unique n with cells contianing v) to long format (column n containing attribute name and column v containing attribute value).
#'  \item edges are connected via their "s"art and "t"arget fields. The "@id" and "i"nteraction attribute are stored as is and all edgeAttributes are saved as node attributes, access via rcx[["edgeAttributes"]]. 
#'        Data goes from wide format (columns for each unique n with cells contianing v) to long format (column n containing attribute name and column v containing attribute value).
#' }

#' 
#' @param ngraph ngraph object
#' @param verbose logical; whether to print out extended feedback 
#' @return returns object of class RCX if successfull, NULL otherwise
#' @seealso \code{\link{ngraph}} \code{\link{ndex.RCX2ngraph}} \code{\link{ndex.JSON2RCX}} \code{\link{ndex.RCX2JSON}}   
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.complete.network(ndexcon,pws[1,"externalId"]) 
#' ngraph = ndex.RCX2ngraph(rcx) 
#' rcxback = ndex.ngraph2RCX(ngraph)
#' 
#' #test equalness
#' for(i in names(rcx)) {
#' cat(i)
#' cat(all.equal(rcx[[i]], rcxback[[i]]))
#' cat("\n")
#' }
#' }
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
    tmp2 = as.data.frame(tmp$vertices[,"name"], stringsAsFactors=F, row.names = NULL)
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
      tmp2 = as.data.frame(tmp$vertices[,"name"], stringsAsFactors=F, row.names = NULL)
      colnames(tmp2) = c("po")
      tmp2 = as.data.frame(cbind(tmp2, tmp$vertices[,sel,drop=FALSE]), stringsAsFactors=F)
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
      tmp2 = as.data.frame(cbind(tmp2, tmp$edges[,sel,drop=FALSE]), stringsAsFactors=F)
      row.names(tmp2) = NULL
      tmp2 = tidyr::gather_(tmp2,"n","v",colnames(tmp$edges)[sel])
      tmp2 = plyr::arrange(tmp2,po)
      tmp2$d = "list_of_string"
      aspectlist[["edgeAttributes"]] = tmp2
    }
  }

  return(aspectlist)
  
}

