\name{ndexgraph2cbdd}
\alias{ndexgraph2cbdd}
\title{Get basic network eligible for CBDD from \code{\link{ndexgraph}} object}
\usage{
  ndexgraph2cbdd(g, useNamespace)
}
\arguments{
  \item{g}{\code{\link{ndexgraph}} object}

  \item{useNamespace}{(optional) name of the namespace for
  nodes}
}
\value{
  data frame with two columns containing IDs of interacting
  nodes
}
\description{
  Get basic network eligible for CBDD from
  \code{\link{ndexgraph}} object
}
\details{
  By default, node IDs will be the references from the
  basic terms ('represents' component of Node) If
  useNamespace is supplied, function will try to reencode
  nodes with given namespace using aliases and related
  terms stored in \code{node_annot} slot of input
}

