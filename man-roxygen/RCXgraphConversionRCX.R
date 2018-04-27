#' @description This function creates an \code{\link{RCX}} object from a valid <%= ifelse(exists("graphObject"), graphObject, "RCXgraph") %> object.
#'
#' The following rules apply to convert from \code{\link{RCXgraph}} to \code{\link{RCX}}:
#' \itemize{
#'  \item all graph attributes are stored as named data.frames within the RCX object
#'  \item nodes receive their name value as "@id" attribute. All other node attributes are saved in the RCX object as nodeAttributes, access via rcx[["nodeAttributes"]].
#'        Data goes from wide format (columns for each unique n with cells contianing v) to long format (column n containing attribute name and column v containing attribute value).
#'  \item edges are connected via their "s"art and "t"arget fields. The "@id" and "i"nteraction attribute are stored as is and all edgeAttributes are saved as node attributes, access via rcx[["edgeAttributes"]].
#'        Data goes from wide format (columns for each unique n with cells contianing v) to long format (column n containing attribute name and column v containing attribute value).
#' }
