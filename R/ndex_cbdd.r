##Authors:
#   Frank Kramer [frank.kramer@med.uni-goettingen.de]
## Created: 05 Oct 2016
## Base functions to create CBDD-like data from RCX and ngraph objects


#' Create CBDD-like data from \code{\link{RCX}} object
#' 
#' Returns CBDD-like data as defined at https://cbdd.thomsonreuterslifesciences.com/cbdd/help_algorithms/
#' The identifiers used are (in this order, if applicable) the namespace ids, the "r"epresents field, the "n"ame field or the @id field of the CX data
#' 
#' @param rcx \code{\link{RCX}} object
#' @param useNamespace string (optional) name of the namespace for nodes
#' @return data.frame with two columns containing IDs of interacting nodes. Returns NULL if rcx object does not contain edge information.
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.complete.network(ndexcon,pws[1,"externalId"]) 
#' cbdd = ndex.RCX2CBDD(rcx)}
#' @export
ndex.RCX2CBDD <- function(rcx, useNamespace = NULL){

  if(!("RCX" %in% class(rcx))) stop("ndex.RCX2CBDD: supplied parameter rcx not of class RCX")
  
  if(is.null(rcx$edges) || dim(rcx$edges[,c("s","t")])[1] == 0) {
    warning("ndex.RCX2CBDD: rcx object does not contain edge information. Returning NULL.")
    return(NULL)
  }
  
  if("i" %in% colnames(rcx$edges)) { # if CX data contains interaction type information put this into mechanism column
    cbdd <- rcx$edges[,c("s","t","i")]
    colnames(cbdd) = c("node1","node2","mechanism")
  } else {
    cbdd <- rcx$edges[,c("s","t")]
    colnames(cbdd) = c("node1","node2")
  }
  
  ## reencode numeric ids
  usedNamespace = FALSE
  if(!is.null(useNamespace)) {
    ## getting namespace info doesnt work atm
    
    usedNamespace = FALSE
  }
  
  if(!usedNamespace) {
    if ("r" %in% colnames(rcx$nodes)) {
      cbdd$node1 = rcx$nodes$r[match(cbdd$node1, rcx$nodes[,"@id"])]
      cbdd$node2 = rcx$nodes$r[match(cbdd$node2, rcx$nodes[,"@id"])]
    } else if ("n" %in% colnames(rcx$nodes)) {
      cbdd$node1 = rcx$nodes$n[match(cbdd$node1, rcx$nodes[,"@id"])]
      cbdd$node2 = rcx$nodes$n[match(cbdd$node2, rcx$nodes[,"@id"])]
    }
  }

  return(cbdd)
}

#' Create CBDD-like data from \code{\link{ngraph}} object
#' 
#' Returns CBDD-like data as defined at https://cbdd.thomsonreuterslifesciences.com/cbdd/help_algorithms/
#' The identifiers used are (in this order, if applicable) the namespace ids, the "r"epresents field, the "n"ame field or the @id field of the CX data
#' 
#' Internally this function calls ndex.ngraph2RCX and then ndex.RCX2CBDD.
#' 
#' @param ngraph \code{\link{ngraph}} object
#' @param useNamespace string (optional) name of the namespace for nodes
#' @return data.frame with two columns containing IDs of interacting nodes. Returns NULL if ngraph object does not contain edge information.
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.complete.network(ndexcon,pws[1,"externalId"]) 
#' ngraph = ndex.RCX2ngraph(rcx) 
#' cbdd = ndex.ngraph2CBDD(ngraph)}
#' @export
ndex.ngraph2CBDD <- function(ngraph, useNamespace = NULL){
  
  if(!("igraph" %in% class(ngraph))) stop("ndex.ngraph2CBDD: supplied parameter ngraph not of class igraph or ngraph")
  
  return( ndex.RCX2CBDD(ndex.ngraph2RCX(ngraph),useNamespace) )

}
