#' The ndexr package offers an interface to NDEx servers, e.g. the public server at http://ndexbio.org/. It can retrieve and save networks via the API. Networks are offered as RCX object and as igraph representation. 
#'
#' \tabular{ll}{
#' Package: \tab ndexr\cr
#' Type: \tab Package\cr
#' Version: \tab 2.0.7\cr
#' Date: \tab 2016-12-02\cr
#' License: \tab TBD\cr
#' }
#'
#' @author Frank Kramer \email{frank.kramer@informatik.uni-augsburg.de}
#' @author Florian Auer \email{florian.auer@informatik.uni-augsburg.de}
#' @author Alex Ishkin \email{aleksandr.ishkin@thomsonreuters.com}
#' @author Dexter Pratt \email{depratt@ucsc.edu}
#' @name ndexr-package
#' @aliases ndexr
#' @docType package
#' @title NDEx R client library
#' @keywords package
#' @examples
#' \dontrun{
#' require(ndexr)
#' ###connect anonymously
#' ndexcon1 = ndex_connect(verbose=T)
#' ###get network api
#' apidata1 = ndex_get_network.api(ndexcon1)
#' ###find some networks containing p53
#' pws1 = ndexr::ndex_find_networks(ndexcon1,"p53")
#' ###get complete network as RCX
#' rcx1 = ndex.get.complete.network(ndexcon1,pws1[1,"externalId"])
#' ###convert to rcxgraph and back
#' rcxgraph1 = rcx_toRCXgraph(rcx1)
#' plot(rcxgraph1, vertex.label=V(rcxgraph1)$n, edge.label=E(rcxgraph1)$i)
#' }
NULL
