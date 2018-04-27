#' @description This function creates an <%= ifelse(exists("graphObject"), graphObject, "RCXgraph") %> object from a supplied \code{\link{RCX}} object.
#' RCX objects store the CX data as a named list of data.frames containing metaData and all aspects of the network.
#' The <%= ifelse(exists("graphObject"), graphObject, "RCXgraph") %> class inherits from igraph and contains the complete (R)CX information as graph, node and edge attributes.
#' All \code{\link[igraph]{igraph}} functionality is available, e.g. access nodes and edges of igraph g via V(g) and E(g) and their attributes via V(g)$attribute
#'
#' @details 
#' The following rules apply to convert from \code{\link{RCX}} to <%= ifelse(exists("graphObject"), graphObject, "RCXgraph") %>:
#' \itemize{
#'  \item nodes receive their name from RCX$node$n. If idAsVertexName is TRUE, the "@id" value is used as name. All other information in aspects node and nodeAttributes are saved as \code{\link[igraph]{vertex_attr}}, access via V(g).
#'        Data goes from long format (column n containing attribute name and column v containing attribute value) to wide format (columns for each unique n with cells contianing v).
#'  \item edges are connected via their "s"ource and "t"arget fields. The "@id" and "i"nteraction attribute are stored as is and all edgeAttributes are saved as \code{\link[igraph]{edge_attr}}, access via E(g).
#'        Data goes from long format (column n containing attribute name and column v containing attribute value) to wide format (columns for each unique n with cells contianing v).
#'  \item all other aspect data is stored as graph attributes, access via g$<aspectName>
#' }
#' The following rules apply to convert <%= ifelse(exists("graphObject"), graphObject, "RCXgraph") %> back to \code{\link{RCX}}:
#' \itemize{
#'  \item Two vertex attributes "n" and "@id" have to be present in <%= ifelse(exists("graphObject"), graphObject, "RCXgraph") %>! Those two are mandatory \code{\link{RCX}} node properties!
#'  \item The igraph vertex name is ignored for the conversion! If the name is needed, adjust manually, e.g.: V(<%= ifelse(exists("graphObject"), graphObject, "RCXgraph") %>)$n <- V(<%= ifelse(exists("graphObject"), graphObject, "RCXgraph") %>)$name
#'  \item The edge attribute "@id" has to be present in <%= ifelse(exists("graphObject"), graphObject, "RCXgraph") %>! This is a mandatory \code{\link{RCX}} edge property!
#' }
