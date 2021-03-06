% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ndex_networks_aspects_and_metadata.r
\name{ndex_network_aspect_get_metadata}
\alias{ndex_network_aspect_get_metadata}
\title{Get the Metadata Associated with a Network UUID}
\usage{
ndex_network_aspect_get_metadata(ndexcon, networkId, aspect)
}
\arguments{
\item{ndexcon}{object of class NDExConnection link{ndex_connect}}

\item{networkId}{character; unique ID (UUID) of the network}

\item{aspect}{character; aspect name}
}
\value{
metadata for an aspect as list: consistencyGroup, elementCount, lastUpdate, data, name, properties, version and idCounter
}
\description{
This function retrieves the metadata associated with the supplied network UUID.
}
\note{
Compatible to NDEx server version 2.0

Server error (version 2.0) since March 13th 2017
}
\section{REST query}{

GET: ndex_config$api$network$aspect$getMetaDataByName
}

\examples{
## Establish a server connection
ndexcon = ndex_connect()
## Find a network and get its UUID
networks = ndex_find_networks(ndexcon,"p53", "nci-pid")
networkId = networks[1,"externalId"]
## Get the meta-data of an aspect of a network
ndex_network_aspect_get_metadata(ndexcon, networkId, 'nodeAttributes') 
}
