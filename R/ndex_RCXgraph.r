################################################################################
## Authors:
##   Frank Kramer [frank.kramer@med.uni-goettingen.de]
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##   Zaynab Hammoud [zaynab.hammoud@med.uni-goettingen.de]
##
## History:
##   Created on 20 September 2016 by Kramer
##   Copied from NDExR package on 3 August 2017 by Auer
##
## Description:
##    Base functions to create, parse, modify RCXgraph/igraph objects from/to CX networks
################################################################################

#' Create RCXgraph object from RCX object 
#'
#' This function creates an RCXgraph object from a supplied \code{\link{RCX}} object.
#' RCX objects store the CX data as a named list of data.frames containing metaData and all aspects of the network.
#' The RCXgraph class inherits from igraph and contains the complete (R)CX information as graph, node and edge attributes.
#' All \code{\link[igraph]{igraph}} functionality is available, e.g. access nodes and edges of igraph g via V(g) and E(g) and their attributes via V(g)$attribute
#'
#' The following rules apply to convert from \code{\link{RCX}} to RCXgraph:
#' \itemize{
#'  \item nodes receive the "@id" value as name. All other information in aspects node and nodeAttributes are saved as node attributes, access via V(g).
#'        Data goes from long format (column n containing attribute name and column v containing attribute value) to wide format (columns for each unique n with cells contianing v).
#'  \item edges are connected via their "s"art and "t"arget fields. The "@id" and "i"nteraction attribute are stored as is and all edgeAttributes are saved as node attributes, access via E(g).
#'        Data goes from long format (column n containing attribute name and column v containing attribute value) to wide format (columns for each unique n with cells contianing v).
#'  \item all other aspect data is stored as graph attributes, access via g$aspect
#' }
#'
#' An RCXgraph object could look like this:\cr
#'\preformatted{
#'> str(rcxgraph)
#'  IGRAPH DN-B 244 141 --
#'   + attr: metaData (g/x), numberVerification (g/x), ndexStatus (g/x), networkAttributes (g/x), hiddenAttributes (g/x), cartesianLayout (g/x),
#'   | visualProperties (g/x), subNetworks (g/x), cyViews (g/x), networkRelations (g/x), cyTableColumn (g/x), status (g/x), name (v/c), n (v/c), NAME (v/c),
#'   | type (v/c), selected (v/c), name (e/c), i (e/c), shared name (e/c), Score (e/c), NAME (e/c), interaction (e/c), Direction (e/c), Annotation (e/c),
#'   | selected (e/c)
#'  + edges (vertex names):
#'    [1] 314->315 312->313 311->313 310->311 309->313 309->311 308->309 306->307 304->305 302->303 299->301 299->300 297->298 295->296 293->292 291->292 289->290 287->288
#'   [19] 285->286 283->284 281->282 279->280 277->278 275->276 273->274 271->272 269->270 267->268 265->266 263->264 261->262 259->260 257->258 255->256 253->254 251->252
#'   [37] 249->250 247->248 245->246 242->243 240->241 238->239 236->237 234->235 232->233 230->231 228->229 226->227 224->225 224->221 223->221 222->221 220->221 218->217
#'   [55] 216->217 214->294 214->244 214->215 212->213 210->211 208->209 206->207 204->205 202->203 200->201 199->198 198->241 198->235 197->198 195->196 194->196 193->194
#'   [73] 192->194 192->193 191->194 191->193 190->194 190->191 188->189 186->187 184->185 182->183 180->181 179->178 177->219 177->178 175->176 173->174 171->172 169->170
#'   [91] 167->168 165->166 163->164 161->162 160->159 158->159 156->157 155->217 154->155 152->153 150->151 148->149 146->147 144->145 143->198 142->143 140->141 138->139
#'  [109] 136->137 134->135 132->133 130->131 128->129 126->127 124->125 122->123 120->121 118->119 116->117 114->115 112->113 110->111 108->109 106->107 104->105 102->103
#'  [127] 100->101 98 ->99  96 ->97  94 ->95  92 ->93  90 ->91  88 ->89  86 ->87  84 ->85  82 ->83  80 ->81  78 ->79  76 ->77  74 ->75  72 ->73
#'
#'> V(rcxgraph)[[]]   ## @id as vertex names
#'  + 244/244 vertices, named:
#'      name                         n                      NAME type selected
#'  1    315                     ZNF84                     ZNF84 gene    false
#'  2    314 chr12:133450233-133450289 chr12:133450233-133450289   nc    false
#'  3    313                    ZNF572                    ZNF572 gene    false
#'  4    312  chr8:125985493-125985548  chr8:125985493-125985548   nc    false
#'  5    311                    ZNF561                    ZNF561 gene    false
#'  ....
#'
#'> V(rcxgraph)$NAME   ## (node) names of the vertices from rcx$nodeAttributes
#'   [1] "ZNF84"                     "chr12:133450233-133450289" "ZNF572"                    "chr8:125985493-125985548"  "ZNF561"
#'   ...
#'
#'> E(rcxgraph)[[]]
#'  + 11/11 edges (vertex names):
#'  + 141/141 edges (vertex names):
#'      tail head tid hid name              i                                         shared.name Score                                                NAME    interaction
#'  1    315  314   1   2  456 interacts with    chr12:133450233-133450289 (interacts with) ZNF84     1    chr12:133450233-133450289 (interacts with) ZNF84 interacts with
#'  2    313  312   3   4  455 interacts with    chr8:125985493-125985548 (interacts with) ZNF572     1    chr8:125985493-125985548 (interacts with) ZNF572 interacts with
#'  3    313  311   3   5  454 interacts with                      ZNF561 (interacts with) ZNF572  <NA>                      ZNF561 (interacts with) ZNF572 interacts with
#'  4    311  310   5   6  453 interacts with       chr19:9707546-9707590 (interacts with) ZNF561     1       chr19:9707546-9707590 (interacts with) ZNF561 interacts with
#'  5    313  309   3   7  452 interacts with                      ZNF257 (interacts with) ZNF572  <NA>                      ZNF257 (interacts with) ZNF572 interacts with
#'
#'> E(rcxgraph)$i
#'  [1] "neighbor-of"                 "neighbor-of"                 "neighbor-of"
#'  [4] "controls-expression-of"      "controls-phosphorylation-of" "controls-state-change-of"
#'  [7] "controls-phosphorylation-of" "controls-state-change-of"    "controls-phosphorylation-of"
#'  [10] "controls-state-change-of"    "neighbor-of"
#'}
#'
#'
#' @param rcx RCX object
#' @param verbose logical; whether to print out extended feedback
#'
#' @return returns object of class RCXgraph if successfull, NULL otherwise
#'
#' @seealso \code{\link{rcxgraph_toRCX}} \code{\link{rcx_fromJSON}} \code{\link{rcx_toJSON}} \code{\link{RCX}} \code{\link[igraph]{igraph}}
#' @aliases RCXgraph
#' @examples
#' ## Create an RCX object
#' rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))
#' ## Convert to RCXgraph
#' rcxgraph = rcxgraph_fromRCX(rcx)
#' @export
rcxgraph_fromRCX <- function(rcx, verbose = FALSE){

  if(!("RCX" %in% class(rcx))) {
    warning("RCX2RCXgraph: supplied parameter is not of class RCX! Returning null.")
    return(NULL)
  }

  ##### create empty graph
  rcxgraph = igraph::make_empty_graph()
  # adding graph attributes is harmless
  rcxgraph = ndex_internal_addAspects(rcxgraph, rcx, verbose)

  ## sanity checks: no nodes defined
  if(is.null(rcx$nodes) || dim(rcx$nodes)[1] == 0) {
    warning("RCX2RCXgraph: supplied RCX does not contain node information. Returning RCXgraph object without nodes or edges.")
    return(rcxgraph)
  }

  ## add nodes and nodeAttributes (if available)
  ids = as.character(rcx$nodes$"@id")
  nodes = as.list(rcx$nodes[names(rcx$nodes)!='@id'])
  rcxgraph = igraph::add_vertices(rcxgraph, length(ids), name=ids, attr=nodes)
  if(!is.null(rcx$nodeAttributes) && dim(rcx$nodeAttributes)[1] > 0) {
    for(attrname in unique(rcx$nodeAttributes$n)) {
      sel = rcx$nodeAttributes$n == attrname
      ## in igraph the attribute 'name' is used for the id (here @id) of the node, therefore the attribute name has to be changed!
      if(attrname=='name') attrname='NAME'
      ## !ToDo: value type is missing: e.g. n=selected, v=false, d=boolean
      ## Therefore the encoding of list_of... might be incorrect!
      if(verbose){ message('___________\nRCXgraph:\n\tattrname:',attrname, '\n\tindex: ',index=paste(as.character(rcx$nodeAttributes$po[sel]),collapse = ', '),'\n\tvalue: ', value=paste(rcx$nodeAttributes$v[sel],collapse = ', '),'\n') }
      rcxgraph = igraph::set_vertex_attr(rcxgraph,attrname, index=as.character(rcx$nodeAttributes$po[sel]), value=rcx$nodeAttributes$v[sel])
    }
  }

  ## sanity checks: no edges defined
  if(is.null(rcx$edges) || dim(rcx$edges)[1] == 0) {
    warning("RCX2RCXgraph: supplied RCX does not contain edge information. Returning RCXgraph object without edges.")
    return(rcxgraph)
  }

  # add edges and edgeAttributes (if available)
  edges = as.character(c(t(rcx$edges[,c("s","t")])))
  edgeNames = as.character(rcx$edges$'@id')
  edgeAttr = rcx$edges
  rownames(edgeAttr) = edgeAttr$'@id'
  edgeAttr$s = NULL
  edgeAttr$t = NULL
  edgeAttr$'@id' = NULL
  #rcxgraph = igraph::add_edges(rcxgraph,edges=c(t(rcx$edges[,c("s","t")])),attr=edgeAttr)
  #rcxgraph = igraph::add_edges(rcxgraph,edges=c(t(rcx$edges[,c("s","t")])),attr=rcx$edges[,c("@id","i")])
  #rcxgraph = igraph::add_edges(rcxgraph,edges=c(t(rcx$edges[,c("s","t")])))

  rcxgraph = igraph::add_edges(rcxgraph, edges=edges, name=edgeNames, attr=edgeAttr)
  if(!is.null(rcx$edgeAttributes) && dim(rcx$edgeAttributes)[1] > 0) {
    for(attrname in unique(rcx$edgeAttributes$n)) {
      sel = rcx$edgeAttributes$n == attrname
      ## in igraph the attribute 'name' is used for the id (here @id) of the node, therefore the attribute name has to be changed!
      if(attrname=='name') attrname='NAME'
      if(verbose){ message('___________\nRCXgraph:\n\tattrname:',attrname, '\n\tindex: ',index=paste(as.character(rcx$edgeAttributes$po[sel]),collapse = ', '),'\n\tvalue: ', value=paste(rcx$edgeAttributes$v[sel],collapse = ', '),'\n') }
      rcxgraph = igraph::set_edge_attr(rcxgraph, attrname, index=as.character(rcx$edgeAttributes$po[sel]), value=rcx$edgeAttributes$v[sel])
    }
  }

  class(rcxgraph) = c("RCXgraph",class(rcxgraph))
  return(rcxgraph)

}


#' Create RCXgraph object from RCX object
#'
#' @param rcx RCX object
#' @param verbose logical; whether to print out extended feedback
#'
#' @return returns object of class RCXgraph if successfull, NULL otherwise
#'
#' @note Wrapper function for \code{\link{rcxgraph_fromRCX}}
#' @examples
#' ## Create an RCX object
#' rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))
#' ## Convert to RCXgraph
#' RCXgraph = rcx_toRCXgraph(rcx)
#' @export
rcx_toRCXgraph <- rcxgraph_fromRCX


#' ndex_internal_addAspects
#'
#' @param rcxgraph RCXgraph object
#' @param rcx RCX object
#' @param verbose logical; whether to print out extended feedback
#' @return returns object of class RCXgraph
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


#' Create RCX object from RCXgraph object
#'
#' This function creates an RCX object from a valid RCXgraph object.
#'
#' The following rules apply to convert from \code{\link{RCXgraph}} to \code{\link{RCX}}:
#' \itemize{
#'  \item all graph attributes are stored as named data.frames within the RCX object
#'  \item nodes receive their name value as "@id" attribute. All other node attributes are saved in the RCX object as nodeAttributes, access via rcx[["nodeAttributes"]].
#'        Data goes from wide format (columns for each unique n with cells contianing v) to long format (column n containing attribute name and column v containing attribute value).
#'  \item edges are connected via their "s"art and "t"arget fields. The "@id" and "i"nteraction attribute are stored as is and all edgeAttributes are saved as node attributes, access via rcx[["edgeAttributes"]].
#'        Data goes from wide format (columns for each unique n with cells contianing v) to long format (column n containing attribute name and column v containing attribute value).
#' }

#'
#' @param rcxgraph RCXgraph object
#' @param verbose logical; whether to print out extended feedback
#' @return returns object of class RCX if successfull, NULL otherwise
#' @seealso \code{\link{RCXgraph}} \code{\link{rcxgraph_fromRCX}} \code{\link{rcx_fromJSON}} \code{\link{rcx_toJSON}}
#' @examples
#' ## Create an RCX object
#' rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))
#' ## Convert to RCXgraph
#' rcxgraph = rcxgraph_fromRCX(rcx)
#' ## Convert it back to rcx
#' rcx = rcxgraph_toRCX(rcxgraph)
#' @export
rcxgraph_toRCX <- function(rcxgraph, verbose = FALSE){

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
    tmp2 = as.data.frame(tmp$vertices[,"name"], stringsAsFactors=FALSE, row.names = NULL)
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

