\name{ndex_rest_GET}
\alias{ndex_rest_GET}
\title{Generic GET query to API}
\usage{
  ndex_rest_GET(route, auth = TRUE)
}
\arguments{
  \item{route}{Character (route to specific REST query)}

  \item{auth}{Logical: is authentication required?}
}
\value{
  JSON response from REST server (it will be handled
  downstream)
}
\description{
  Generic GET query to API
}
\details{
  Simply execute HTTP GET on URL host/route and fetch
  whatever data REST server returns Making sure the route
  is well-formed is the job of calling function
}
\examples{
\dontrun{ndex_rest_GET("/networks/api")}
}
\seealso{
  \code{\link{_ndex_rest_PUT}},
  \code{\link{_ndex_rest_POST}},
  \code{\link{_ndex_rest_DELETE}}
}

