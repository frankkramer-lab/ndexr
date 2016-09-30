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

I'm in a hurry, give me the tl;dr
--------------

```
library(ndexr)
###connect anonymously
ndexcon1 = ndex.connect(verbose=T)
###connect as test user
ndexcon2 = ndex.connect(username="testacc", password="testacc", verbose=T)

###get network api - skip this you wont need it
apidata1 = ndex.get.network.api(ndexcon1)

###find some networks containing p53
pws1 = ndexr::ndex.find.networks(ndexcon1,"p53")

###get network details
pwsummary1 = ndex.get.network.summary(ndexcon1,pws1[1,"externalId"])

###get complete network as RCX object
rcx1 = ndex.get.complete.network(ndexcon1,pws1[1,"externalId"])

###plot it - no beauty here yet
plot(ngraph1, vertex.label=V(ngraph1)$n, edge.label=E(ngraph1)$i)

###convert to ngraph and back
ngraph1 = ndex.RCX2ngraph(rcx1)
rcx_back1 = ndex.ngraph2RCX(ngraph1)

##test equality of conversion:
for(i in names(rcx1)) {
  cat(i)
  cat(all.equal(rcx1[[i]], rcx_back1[[i]]))
  cat("\n")
}

#work on!
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
* ndex.get.network.provenance, ndex.set.network.readonly, ndex.get.limited.aspect, ndex.get.network.namespace, ndex.get.network.metadata
* ndex.get.complete.network -> ndex.RCX2ngraph -> ndex.ngraph2RCX -> ndex.RCX2JSON round trip works


### What is broken from prev versions:
* class `ndexgraph` : use classes RCX and ngraph now
* Conversion of NDEx network object to the data frame usable by CBDD package

### What has changed:
* Code clean up: 
** produce text messages via message() instead of warning() or cat()
** store connection infos in an object (NDExConnection) instead of enviromen. this enables re-use of connection between sessions and between multiple NDEx servers
* transferred basic network functions from RJSONIO to jsonlite
* Low-level functions (GET, POST and PUT) have a swtich to return the raw response, without call to jsonlite::fromJSON
* Networks are retrieved using the asCX API calls
* RCX objects store the received CX data as a list of data.frames
* ngraph objects represent the CX-encoded networks as graph (see "ndex.RCX2ngraph" and "ndex.ngraph2RCX")

### What is being worked on:
* Start with implementing all API calls which produce data. Stay with 
the raw data mostly. This is very close.
* Implement the CX data model to go from JSON to CX  and back. This is prototyped.
* Implement ngraph object extending igraph class to go from RCX to ngraph and back.  This is prototyped.
* Implement the API calls which save data on the server. This is being debugged.

### Implementation details

The package is fully functional; can be installed and tried.

Documentation is done using [roxygen2]() package.

Unit tests are not created so far.

HTTP requests are performed using [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html) package

Parsing and creation of JSON is done using [jsonlite]() package.

The ngraph object inherits from igraph - which has lots and lots of functionality. 
See https://cran.r-project.org/web/packages/igraph/index.html and http://igraph.org/r/ 

### RCX 

The RCX object is currently implemented within this package as a list of data.frames containing metaData and all aspects of the network.
The structure of an RCX object, as shown via 'str(rcx)' could be a list like this:
 
```
 > str(rcx)
 
 List of 12
 $ metaData          :'data.frame':	11 obs. of  7 variables:
   ..$ name            : chr [1:11] "citations" "@context" "edgeAttributes" "edgeCitations" ...
   ..$ consistencyGroup: int [1:11] 1 1 1 1 1 1 1 1 1 1 ...
   ..$ elementCount    : int [1:11] 4 23 NA NA 11 1 NA NA NA 5 ...
   ..$ lastUpdate      : num [1:11] 1.44e+12 1.44e+12 1.44e+12 1.44e+12 1.44e+12 ...
   ..$ version         : chr [1:11] "1.0" "1.0" "1.0" "1.0" ...
   ..$ idCounter       : int [1:11] 60714397 NA NA NA 60714399 NA NA NA NA 60714395 ...
   ..$ properties      :List of 11
 $ numberVerification:'data.frame':	1 obs. of  1 variable:
   ..$ longNumber: num 2.81e+14
 $ ndexStatus        :'data.frame':	1 obs. of  10 variables:
   ..$ externalId      : chr "eac8a4b8-6194-11e5-8ac5-06603eb7f303"
   ..$ creationTime    : num 1.44e+12
   ..$ modificationTime: num 1.44e+12
   ..$ visibility      : chr "PUBLIC"
   ..$ published       : logi FALSE
   ..$ nodeCount       : int 5
   ..$ edgeCount       : int 11
   ..$ owner           : chr "nci-pid"
   ..$ ndexServerURI   : chr "http://public.ndexbio.org"
   ..$ readOnly        : logi FALSE
 $ @context          :'data.frame':	1 obs. of  23 variables:
   ..$ GENPEPT                      : chr "http://www.ncbi.nlm.nih.gov/protein/"
   ..$ NCBI GENE                    : chr "http://identifiers.org/ncbigene/"
   ..$ ENSEMBL                      : chr "http://identifiers.org/ensembl/"
   [...]
 $ networkAttributes :'data.frame':	4 obs. of  2 variables:
   ..$ n: chr [1:4] "name" "description" "version" "ndex:sourceFormat"
   ..$ v: chr [1:4] "PLK3 signaling events" "This network ..." [...]
 $ citations         :'data.frame':	4 obs. of  7 variables:
  ..$ @id           : int [1:4] 60714380 60714383 60714386 60714397
  ..$ dc:identifier : chr [1:4] "pmid:17264206" "pmid:14968113" "pmid:12242661" "pmid:11551930"
  ..$ dc:type       : chr [1:4] "URI" "URI" "URI" "URI"
  ..$ attributes    :List of 4 [...]
 $ nodes             :'data.frame':	5 obs. of  2 variables:
  ..$ @id: int [1:5] 60714376 60714377 60714381 60714384 60714395
  ..$ n  : chr [1:5] "CCNE1" "PLK3" "MPIP3" "CHK2" ...
 $ nodeAttributes    :'data.frame':	10 obs. of  4 variables:
   ..$ po: int [1:10] 60714376 60714376 60714377 60714377 60714381 60714381 60714384 60714384 60714395 60714395
   ..$ n : chr [1:10] "alias" "relatedTo" "alias" "relatedTo" ...
   ..$ v :List of 10
     .. ..$ : chr [1:6] "UniProt Knowledgebase:Q92501" "UniProt Knowledgebase:Q9UD21"  ...
     .. ..$ : chr [1:98] "GENE ONTOLOGY:GO:0003713" "GENE ONTOLOGY:GO:0005515"  ...
     [...]
   ..$ d : chr [1:10] "list_of_string"  ...
 $ edges             :'data.frame':	11 obs. of  4 variables:
   ..$ @id: int [1:11] 60714379 60714382  ...
   ..$ s  : int [1:11] 60714376 60714381  ...
   ..$ t  : int [1:11] 60714377 60714377  ...
   ..$ i  : chr [1:11] "neighbor-of" "neighbor-of"  ...
 $ edgeCitations     :'data.frame':	11 obs. of  2 variables:
   ..$ po       :List of 11
   .. ..$ : int 60714379
   .. ..$ : int 60714382
   [...]
 ..$ citations:List of 11
 .. ..$ : int 60714380
 .. ..$ : int 60714383
   [...]
 $ status            :'data.frame':	1 obs. of  2 variables:
   ..$ error  : chr ""
   ..$ success: logi TRUE
- attr(*, "class")= chr [1:2] "RCX" "list"
```
 
The data.frames representing nodes and edges could look like this:

```
 > rcx[["nodes"]]
    @id     n
  1 60714376 CCNE1
  2 60714377  PLK3
  3 60714381 MPIP3
  4 60714384  CHK2
  5 60714395   P53

 > rcx[["edges"]]
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
```


### ngraph:

The ngraph class inherits from igraph and contains the complete (R)CX information as graph, node and edge attributes. 
RCX objects store the CX data as a named list of data.frames containing metaData and all aspects of the network.
The ngraph class inherits from igraph and contains the complete (R)CX information as graph, node and edge attributes.

All igraph functionality is available, e.g. access nodes and edges of igraph g via V(g) and E(g) and their attributes via V(g)$attribute
 
 The following rules apply to convert from RCX to ngraph:
 
 * nodes receive the "@id" value as name. All other information in aspects node and nodeAttributes are saved as node attributes, access via V(g). Data goes from long format (column n containing attribute name and column v containing attribute value) to wide format (columns for each unique n with cells contianing v).
 * edges are connected via their "s"art and "t"arget fields. The "@id" and "i"nteraction attribute are stored as is and all edgeAttributes are saved as node attributes, access via E(g). Data goes from long format (column n containing attribute name and column v containing attribute value) to wide format (columns for each unique n with cells contianing v).
 * all other aspect data is stored as graph attributes, access via g$aspect

An ngraph object could look like this:
```
> str(ngraph)
IGRAPH DN-- 5 11 -- PLK3 signaling events
+ attr: name (g/c), description (g/c), version (g/c), ndex:sourceFormat (g/c), name (v/c), @id (v/n), n
| (v/c), test (v/c), relatedTo (v/x), @id (e/n), i (e/c)
+ edges (vertex names):
 [1] 60714376->60714377 60714381->60714377 60714384->60714377 60714377->60714376 60714377->60714381 60714377->60714381
 [7] 60714377->60714384 60714377->60714384 60714377->60714395 60714377->60714395 60714377->60714395
> V(ngraph)
+ 5/5 vertices, named:
[1] 60714376 60714377 60714381 60714384 60714395
> V(ngraph)$n
[1] "CCNE1" "PLK3"  "MPIP3" "CHK2"  "P53"
> E(ngraph)
+ 11/11 edges (vertex names):
 [1] 60714376->60714377 60714381->60714377 60714384->60714377 60714377->60714376 60714377->60714381 60714377->60714381
 [7] 60714377->60714384 60714377->60714384 60714377->60714395 60714377->60714395 60714377->60714395
> E(ngraph)$i
 [1] "neighbor-of"                 "neighbor-of"                 "neighbor-of"                
 [4] "controls-expression-of"      "controls-phosphorylation-of" "controls-state-change-of"   
 [7] "controls-phosphorylation-of" "controls-state-change-of"    "controls-phosphorylation-of"
[10] "controls-state-change-of"    "neighbor-of"                
```



Examples
--------------

Examples of working with NDEx using `ndexr` package can be found in the tests_local.R file in this repo.


Random thoughts and notes to self
--------------

### metaData: 

pre- and post-metaData can have a different structure but the same JSON name. (see below)

```
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
```

-> take care when joining. currently pre and post are merged with the duplicate properties field removed afterwards

