################################################################################
## Authors:
##   Frank Kramer [frank.kramer@med.uni-goettingen.de]
##
## History:
##   Created on 05 October 2016 by Kramer
##     
## Description:
##   Base functions to create CBDD-like data from RCX and ngraph objects
################################################################################


#' Create CBDD-like data from \code{\link{RCX}} object
#' 
#' Returns CBDD-like data as defined at https://cbdd.thomsonreuterslifesciences.com/cbdd/help_algorithms/
#' The identifiers used are (in this order, if applicable) the namespace ids, the "r"epresents field, the "n"ame field or the @id field of the CX data
#' 
#' @param rcx \code{\link{RCX}} object
#' @param useNamespace string (optional) name of the namespace for nodes
#' @return data.frame with two columns containing IDs of interacting nodes. Returns NULL if rcx object does not contain edge information.
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' ## Find a network and get its UUID
#' networks = ndex.find.networks(ndexcon,"p53")
#' networkId = networks[1,"externalId"]
#' ## Get the network data 
#' rcx = ndex.get.network(ndexcon, networkId) 
#' ## Convert to cbbd
#' cbdd = cbdd.fromRCX(rcx)
#' @export
cbdd.fromRCX <- function(rcx, useNamespace = NULL){

  if(!("RCX" %in% class(rcx))) stop("cbdd.fromRCX: supplied parameter rcx not of class RCX")
  
  if(is.null(rcx$edges) || dim(rcx$edges[,c("s","t")])[1] == 0) {
    warning("cbdd.fromRCX: rcx object does not contain edge information. Returning NULL.")
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
#' Internally this function calls ngraph.toRCX and then cbdd.fromRCX.
#' 
#' @param ngraph \code{\link{ngraph}} object
#' @param useNamespace string (optional) name of the namespace for nodes
#' @return data.frame with two columns containing IDs of interacting nodes. Returns NULL if ngraph object does not contain edge information.
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' ## Find a network and get its UUID
#' networks = ndex.find.networks(ndexcon,"p53")
#' networkId = networks[1,"externalId"]
#' ## Get the network data 
#' rcx = ndex.get.network(ndexcon, networkId) 
#' ## Convert RCX to NGraph
#' ngraph = ngraph.fromRCX(rcx) 
#' ## Convert NGraph to cbdd
#' cbdd = cbdd.fromNGraph(ngraph)
#' @export
cbdd.fromNGraph <- function(ngraph, useNamespace = NULL){
  
  if(!("igraph" %in% class(ngraph))) stop("cbdd.fromNGraph: supplied parameter ngraph not of class igraph or ngraph")
  
  return( cbdd.fromRCX(ngraph.toRCX(ngraph),useNamespace) )

}
