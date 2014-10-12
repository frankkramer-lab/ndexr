\name{ndex.find.networks}
\alias{ndex.find.networks}
\title{Search networks in NDEx (by description)}
\usage{
  ndex.find.networks(searchString, searchType = "contains",
    skip = 0, top = 10)
}
\arguments{
  \item{searchString}{string by which to search}

  \item{searchType}{string; type of search (should be one
  of "exact-match", "contains", "begins-with")}

  \item{skip}{how many networks to skip}

  \item{top}{how many networks to show}
}
\value{
  Data frame with network information: ID, name, whether it
  is public, edge and node count; source and format of
  network
}
\description{
  Search networks in NDEx (by description)
}
\note{
  Search strings may be structured
}
\examples{
\dontrun{ndex.find.networks("calmodulin")}
}

