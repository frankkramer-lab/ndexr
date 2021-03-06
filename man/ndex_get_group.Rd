% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ndex_groups.R
\name{ndex_get_group}
\alias{ndex_get_group}
\title{Get a Group}
\usage{
ndex_get_group(ndexcon, groupId)
}
\arguments{
\item{ndexcon}{object of class NDExConnection link{ndex_connect}}

\item{groupId}{character; unique ID (UUID) of the group}
}
\value{
list of properties describing the group (externalId, emailAddress, website, etc.). Throws error (404) if group isn't found!
}
\description{
Get a Group
}
\note{
Compatible to NDEx server version 2.0
}
\section{REST query}{

GET: ndex_config$api$group$get
}

\examples{
## Establish a server connection
ndexcon = ndex_connect()
## Find a group
groups = ndex_find_groups(ndexcon,"Ideker Lab")
groupId = groups[1,"externalId"]
## Get group information
group = ndex_get_group(ndexcon, groupId)
}
