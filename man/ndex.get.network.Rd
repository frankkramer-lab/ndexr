\name{ndex.get.network}
\alias{ndex.get.network}
\title{Get network edges by ID}
\usage{
  ndex.get.network(network_id, json = FALSE)
}
\arguments{
  \item{network_id}{unique ID of the network}
}
\value{
  \code{\link{ndexgraph}} object
}
\description{
  Get network edges by ID
}
\details{
  Uses getEdges (this procedure will return complete
  network with all elements) Nodes use primary ID of the
  base term ('represents' element) Edges use primary ID of
  the base term ('predicate', or 'p' element) Mapping table
  for the nodes is retrieved ('alias' and 'related' terms)
  to facilitate conversions/data mapping
}
\note{
  Currently fetches all edges; may be suboptimal for huge
  networks
}

