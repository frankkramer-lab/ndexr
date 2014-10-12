\name{json_parse_network_metadata}
\alias{json_parse_network_metadata}
\title{Convert JSON of network metadata to data frame}
\usage{
  json_parse_network_metadata(nd)
}
\arguments{
  \item{nd}{list with network metadata information (JSON or
  parsed JSON)}
}
\value{
  Data frame with network information: ID, name, whether it
  is public, edge and node count; source and format of
  network
}
\description{
  Convert JSON of network metadata to data frame
}

