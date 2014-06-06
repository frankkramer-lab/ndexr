\name{ndex.get.network.metadata}
\alias{ndex.get.network.metadata}
\title{Get network metadata by ID}
\usage{
  ndex.get.network.metadata(network_id, json = FALSE)
}
\arguments{
  \item{network_id}{unique ID of the network}

  \item{json}{logical; whether to return JSON (TRUE) or
  convert it to data frame. Default FALSE}
}
\value{
  Complete JSON response or Data frame with network
  metadata: ID, name, whether it is public, edge and node
  count; source and format of network
}
\description{
  Get network metadata by ID
}

