\name{ndex_rest_POST}
\alias{ndex_rest_POST}
\title{Generic POST query to API}
\usage{
  ndex_rest_POST(route, ..., auth = TRUE)
}
\arguments{
  \item{route}{Character (route to specific REST query)}

  \item{...}{Whatever data to be supplied with query}

  \item{auth}{Logical: is authentication required?}
}
\value{
  JSON response from REST server (it will be handled
  downstream)
}
\description{
  Generic POST query to API
}
\details{
  Simply execute HTTP PUT on URL host/route and fetch
  whatever data REST server returns Making sure the route
  is well-formed is the job of calling function Making sure
  the data is well-formed is also the job of calling
  function
}
\examples{
##TBD
}
\seealso{
  \code{\link{_ndex_rest_GET}},
  \code{\link{_ndex_rest_PUT}},
  \code{\link{_ndex_rest_DELETE}}
}

