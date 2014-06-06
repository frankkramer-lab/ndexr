ndex-r-client
=============

R client library for NDEx

Installation
--------------

Installation instructions (using [devtools](http://cran.r-project.org/web/packages/devtools/index.html) R package)

```
require(devtools)
install_github("cytoscape/ndex-r-client@ndexr")
require(ndexr)
```

Implementation
--------------

### Implemented functionality so far:

* First attempt to represent NDEx network in R, using tabular representation of nodes, edges etc. (S4 class `ndexgraph`)
* Connection to REST server (`ndex.connect`)
* Low-level functions to run GET, POST and PUT queries to the REST server
* Search networks (`ndex.find.networks`)
* Retrieve network metadata (`ndex.network.metadata(network_id)`)
* Retrieve network (`ndex.network(network_id)`)
* Conversion of NDEx network object to the data frame usable by CBDD package

### Implementation details

The package is fully functional; can be installed and tried.

Documentation is done using [roxygen2]() package (specifically formatted comments in code are automatically converted to Rdoc help files). The package itself doesn't depend on roxygen2.

Unit tests are not created so far.

HTTP requests are performed using [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html) package

Parsing and creation of JSON is done using [RJSONIO](http://cran.r-project.org/web/packages/RJSONIO/index.html) package. Aternatives exist (`rjson`, `jsonlite`), but performance of different approaches has not been benchmarked (plan to do that using [microbenchmark](http://cran.r-project.org/web/packages/microbenchmark/index.html))

Next steps
----------------

Examples of working with NDEx using `ndexr` package can be found in this [Gist](https://gist.github.com/donshikin/6fb7a45c4aa7892673da) or in the `run_test.r` file in the root of repo.

Next steps
----------------

The package is just a start and current implementation is far from perfect. Discussions are needed on the following points

* Representation of network in R (probably changes will be needed to `ndexgraph` class)
* In context of CBDD, the goal is not just to retrieve the network, but also map molecular data onto it and run some analysis. Typically, these molecular data will have the same entity IDs, say Entrez gene IDs. At least test PID networks in dev REST server are hard to work with in this context, as they contain wide diversity of node annotations (proteins, genes, compounds, custom groups). We need to figure out how best to map the external data onto the network retrieved from NDEx. So far I just tried the following scheme:
  * Retrieve NDEx network; store basic terms which are in 'represents' field of Node objects in node attributes table
  * Fetch all other basic terms linked to nodes ('aliases' and 'relatedTerms'); store them in separate `node_annot` slot of a network object
  * Upon conversion to CBDD-compatible network, user can specify namespace to annotate the network nodes. All nodes which have terms in corresponding namespace will be reannotated using data from `node_annot`; others will be left with IDs from default namespace (so user won't be able to map data to them). As a result, following table will be produced:

node1 | node2
------|------
MYC   | CCND1
<non-geneobject> | MYC
...   | ...

