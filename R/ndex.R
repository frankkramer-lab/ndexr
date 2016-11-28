#' The ndexr package offers an interface to NDEx servers, e.g. the public server at http://ndexbio.org/. It can retrieve and save networks via the API. Networks are offered as RCX object and as igraph representation. 
#'
#' \tabular{ll}{
#' Package: \tab ndexr\cr
#' Type: \tab Package\cr
#' Version: \tab 2.0.5\cr
#' Date: \tab 2016-09-29\cr
#' License: \tab TBD\cr
#' }
#'
#' @author Frank Kramer \email{frank.kramer@med.uni-goettingen.de}
#' @author Florian Auer \email{florian.auer@med.uni-goettingen.de}
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
#' ndexcon1 = ndex.connect(verbose=T)
#' ###get network api
#' apidata1 = ndex.get.network.api(ndexcon1)
#' ###find some networks containing p53
#' pws1 = ndexr::ndex.find.networks(ndexcon1,"p53")
#' ###get network details
#' pwsummary1 = ndex.get.network.summary(ndexcon1,pws1[1,"externalId"])
#' ###get complete network as RCX
#' rcx1 = ndex.get.complete.network(ndexcon1,pws1[1,"externalId"])
#' ###convert to ngraph and back
#' ngraph1 = ndex.RCX2ngraph(rcx1)
#' plot(ngraph1, vertex.label=V(ngraph1)$n, edge.label=E(ngraph1)$i)
#' }
NULL
