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
* Defined a prototype RCX class as a named list of data.frames.
* Get network as CX ("ndex.get.complete.network")


### What is broken from prev versions:
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
* Networks are retrieved using the asCX API calls
* RCX objects store the received CX data

### What is being worked on:
* Start with implementing all API calls which produce data. Stay with 
the raw data mostly. This is veryclose.
* Implement the CX data model to go from JSON to CX  and back. This is prototyped.
* Implement ngraph object extending igraph class to go from RCX to ngraph and back.
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


Random thoughts and notes to self
--------------

### metaData: 

pre- and post-metaData can have a different structure but the same JSON name. (see below)

################
aspectlist$metaData
[[1]]
   consistencyGroup elementCount   lastUpdate              name properties version
1                 1           23 1.443031e+12          @context       NULL     1.0
2                 1            4 1.443031e+12         citations       NULL     1.0
3                 1           NA 1.443031e+12    edgeAttributes       NULL     1.0
4                 1           NA 1.443031e+12     edgeCitations       NULL     1.0
5                 1           11 1.443031e+12             edges       NULL     1.0
6                 1            1 1.443031e+12        ndexStatus       NULL     1.0
(....)

[[2]]
  idCounter      name properties
1  60714395     nodes       NULL
2  60714399     edges       NULL
3  60714397 citations       NULL
################

-> take care when joining. currently pre and post are merged with the duplicate properties field removed afterwards

### RCX:

 first prototyped an CX object in R as a list of data.frames, to give you an idea:
##############
rcx
    $numberVerification
       longNumber           1 2.81475e+14
    
     $ndexStatus
                            externalId creationTime modificationTime visibility published nodeCount ...
1 eac8a4b8-6194-11e5-8ac5-06603eb7f303 1.442973e+12     1.443031e+12     PUBLIC   ...

    $citations
       @id dc:title dc:contributor dc:identifier dc:type dc:description attributes
1 60714380                      NA pmid:17264206     URI             NA       NULL
2 60714383                      NA pmid:14968113     URI             NA       NULL
3 60714386                      NA pmid:12242661     URI             NA       NULL
4 60714397                      NA pmid:11551930     URI             NA       NULL

    $nodes
       @id     n
1 60714376 CCNE1
2 60714377  PLK3
3 60714381 MPIP3
4 60714384  CHK2
5 60714395   P53

    $edges
        @id        s        t                           i
1  60714379 60714376 60714377                 neighbor-of
2  60714382 60714381 60714377                 neighbor-of
3  60714385 60714384 60714377                 neighbor-of
4  60714388 60714377 60714376      controls-expression-of
5  60714390 60714377 60714381 controls-phosphorylation-of
6  60714392 60714377 60714381    controls-state-change-of
7  60714393 60714377 60714384 controls-phosphorylation-of
8  60714394 60714377 60714384    controls-state-change-of
9  60714396 60714377 60714395 controls-phosphorylation-of
10 60714398 60714377 60714395    controls-state-change-of
11 60714399 60714377 60714395                 neighbor-of
##############




