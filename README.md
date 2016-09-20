ndexr - The NDEx R Client
=============

(This is a fork of cytoscape/ndex-r-client from 15th Sep 2016 with an almost complete rewrite of the code. Expect scripts based on the previous code to be broken.)


Installation
--------------

Installation instructions (using [devtools](http://cran.r-project.org/web/packages/devtools/index.html) R package)

```
require(devtools)
install_github("frankkramer-lab/ndexr@develop")
library(ndexr)
```

Implementation
--------------

### Implemented functionality so far:

* Connection to REST server (`ndex.connect`, `ndex.alive`)
* List NDEx API calls (`get.network.api`)
* Low-level functions to run GET, POST and PUT queries to the REST server
* Search networks (`ndex.find.networks`)
* Get network information  (`ndex.get.network.summary`)

### What is broken:
* First attempt to represent NDEx network in R, using tabular representation of nodes, edges etc. (S4 class `ndexgraph`)
* Retrieve network metadata (`ndex.network.metadata(network_id)`)
* Retrieve network (`ndex.network(network_id)`)
* Conversion of NDEx network object to the data frame usable by CBDD package

### What has changed:
* Code clean up: 
** produce text messages via message() instead of warning() or cat()
** store connection infos in an object (NDExConnection) instead of enviromen. this enables re-use of connection between sessions and between multiple NDEx servers
* transferred basic network functions from RJSONIO to jsonlite
* Low-level functions (GET, POST and PUT) have a swtich to return the raw response, without call to jsonlite::fromJSON
* 

### What is being worked on:
* we can pull CX data via the call to network/uuid/asCX
* Start with implementing all API calls which produce data. Stay with 
the raw data mostly. This is close.
* Implement the CX data model to go from CX to extended igraph class and 
back.
* Implement the API calls which save data on the server.

### Implementation details

The package is fully functional; can be installed and tried.

Documentation is done using [roxygen2]() package.

Unit tests are not created so far.

HTTP requests are performed using [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html) package

Parsing and creation of JSON is done using [jsonlite]() package.


Examples
--------------

Examples of working with NDEx using `ndexr` package can be found in the tests_local.R file in this repo.


