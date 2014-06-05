\name{ndex.connect}
\alias{ndex.connect}
\title{Connect to NDEx REST API}
\usage{
  ndex.connect(username, password,
    baseroute = "http://dev.ndexbio.org:8080/ndexbio-rest")
}
\arguments{
  \item{baseroute}{URL of NDEx REST server}

  \item{username}{character username}

  \item{password}{character password}
}
\value{
  returns nothing; RCurl options object for authentication
  is stored in the special environment and reused with the
  queries
}
\description{
  Connect to NDEx REST API
}

