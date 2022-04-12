Introduction
============

Networks are a powerful and flexible methodology for expressing
biological knowledge for computation and communication. Albeit its
benefits, the sharing of networks, the collaboration on network
curation, keeping track of changes between different network versions,
and detecting different versions itself, still is a major problem in
network biology.

The Network Data Exchange, or NDEx, is an open-source software framework
to manipulate, store and exchange networks of various types and formats
(Pratt et al., 2015, Cell Systems 1, 302-305, October 28, 2015 ©2015
Elsevier
Inc. [ScienceDirect](http://www.sciencedirect.com/science/article/pii/S2405471215001477)).
NDEx can be used to upload, share and publicly distribute networks,
while providing an output in formats, that can be used by plenty of
other applications.

The public NDEx server is a network data commons which provides pathway
collections like the Pathway Interaction Database of the NCI
(<a href="http://www.ndexbio.org/#/user/301a91c6-a37b-11e4-bda0-000c29202374" class="uri">http://www.ndexbio.org/#/user/301a91c6-a37b-11e4-bda0-000c29202374</a>)
and the Cancer Cell Maps Initiative
(<a href="http://www.ndexbio.org/#/user/b47268a6-8112-11e6-b0a6-06603eb7f303" class="uri">http://www.ndexbio.org/#/user/b47268a6-8112-11e6-b0a6-06603eb7f303</a>).

This package provides an interface to query the public NDEx server, as
well as private installations, in order to upload, download or modify
biological networks.

This document aims to help the user to install and benefit from the wide
range of funtionality of this implementation. The package also provides
classes to implement the Cytoscape Cyberinfrastructure (CX) Format and
to extend the \[iGraph Package\]
(<a href="http://igraph.org/r/" class="uri">http://igraph.org/r/</a>).

The package is compatible with both NDEx versions 1.3 and 2.0.

Installation
============

Installation via Bioconductor
-----------------------------

``` r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("ndexr")
library(ndexr)
```

Installation via GitHub
-----------------------

using
[*devtools*](http://cran.r-project.org/web/packages/devtools/index.html)
R package

``` r
require(devtools)
install_github("frankkramer-lab/ndexr")
library(ndexr)
```

Quick Start
===========

Some short overview of the most important functions

``` r
## load the library!
library(ndexr)
```

``` r
## login to the NDEx server
ndexcon = ndex_connect("username", "password")
```

``` r
## search the networks for "EGFR"
networks <- ndex_find_networks(ndexcon, "EGFR")

## UUID of the first search result
networkId <- networks[1,'externalId']

## get summary of the network
networkSummary <- ndex_network_get_summary(ndexcon, networkId)

## get the entire network as RCX object
rcx <- ndex_get_network(ndexcon, networkId)

## remove NDEx artefacts from network
rcx <- rcx_asNewNetwork(rcx)
```

    ## Warning: 'rcx_asNewNetwork' is deprecated.
    ## Use 'RCX::createRCX()' instead.
    ## See help("Deprecated")

``` r
## do some fancy stuff with the network, then
## update the meta-data
rcx <- rcx_updateMetaData(rcx)
```

    ## Warning: 'rcx_updateMetaData' is deprecated.
    ## Use 'RCX::updateMetaData()' instead.
    ## See help("Deprecated")

``` r
## upload network as a new network to the NDEx server
networkId <- ndex_create_network(ndexcon, rcx)

## do some other fancy stuff with the network, then
## update the network on the server
networkId <- ndex_update_network(ndexcon, rcx)

## realize, you did bad things to the poor network, so better 
## delete it on the server
ndex_delete_network(ndexcon, networkId)
```

Connect to a server
===================

First, establish an connection to the NDEx server. This object is
required for most of the other ndexr functions, because it stores
options and authentication details. It is possible to connect to the
server anonymously or provide a username and password to enable further
functionality. If you have set up your own NDEx server, you might change
the host to your local installation.

``` r
## load the library
library(ndexr)

## connect anonymously
ndexcon = ndex_connect()

## log in with user name and password
ndexconUser = ndex_connect(username="username", password="password")

## specify the server
ndexconLocal = ndex_connect(username="username",
                password="password", 
                host="localhost:8888/ndex/rest")

## manually change the api and connection configuration
ndexcon13 = ndex_connect(ndexConf=ndex_config$Version_1.3)
```

This package is developed following the structure of the documented api
structure. For complete description of the NDEx server api see
[*http://www.home.ndexbio.org/using-the-ndex-server-api/*](http://www.home.ndexbio.org/using-the-ndex-server-api/).
The R functions are named by the category, context and function they
fullfil. In the following, the usage is described in detail, and
hopefully gives a better understanding of logic behind the naming
convention of this package.

Find Networks
=============

To explore or search the networks on an NDEx server, this package offers
a function to retrieve a list of networks from the server. It is
possible to restrict the networks to a specific search string
(e.g. “EGFR”), an account name (only networks of this account will be
shown), or limit the number of fetched networks.

``` r
## list networks on server
networks <- ndex_find_networks(ndexcon) 
## same as previous
networks <- ndex_find_networks(ndexcon, start=0, size=5)

## search for "EGFR"
networksEgfr <- ndex_find_networks(ndexcon, searchString="EGFR")
## same as previous
networksEgfr <- ndex_find_networks(ndexcon, "EGFR")

## same as previous
networksOfUser <- ndex_find_networks(ndexcon, accountName="ndextutorials")
```

As result you get a data.frame containing information of the networks.

``` r
names(networks) 
```

    ##  [1] "ownerUUID"        "isReadOnly"       "subnetworkIds"    "isValid"          "warnings"        
    ##  [6] "isShowcase"       "isCertified"      "indexLevel"       "hasLayout"        "hasSample"       
    ## [11] "cxFileSize"       "cx2FileSize"      "visibility"       "nodeCount"        "edgeCount"       
    ## [16] "completed"        "owner"            "description"      "name"             "properties"      
    ## [21] "externalId"       "isDeleted"        "modificationTime" "creationTime"     "version"

``` r
networks[,c('name','externalId')]
```

    ##          name                           externalId
    ## 1     covid19 a8c0decc-6bbb-11ea-bfdc-0ac135e8bacf
    ## 2  rasmachine cdba5bd5-5195-11e9-9f06-0ac135e8bacf
    ## 3 painmachine e1e5963a-eb0e-11e9-bb65-0ac135e8bacf
    ## 4        prad caa8c3f6-f6a3-11e8-aaa6-0ac135e8bacf
    ## 5        brca a16b5ca0-f6a3-11e8-aaa6-0ac135e8bacf

Simple network operations
=========================

To both, users and networks stored on an NDEx server, a universally
unique identifier (UUID) is assigned. Although both have the same format
(i.e. “xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx”, where x is one of
`[a-z0-9]`), it has to be distinguished between the user UUID and the
network UUID, but the difference is obvious by the context. Within RCX
objects and search results, the network UUID is also referred to as
“externalId” (see previous section). This UUID can be used to access a
network on the server and retrieve just a summary of the network
(similar to the results of a network search) or even the entire network
as RCX object (see next section).

Since networks can contain many nodes and edges, and a huge amount of
other attributes, it is typically advisable to first get a network
summary, to check the node and edge counts for a network before
retrieving the entire network. Thereby the structure of the network
summary is similar the structure of the network list

``` r
## UUID of the first search result
networkId <- networksOfUser[1,'externalId']

## get network summary
networkSummary <- ndex_network_get_summary(ndexcon, networkId)

names(networkSummary)
```

    ##  [1] "ownerUUID"        "isReadOnly"       "subnetworkIds"    "isValid"          "warnings"        
    ##  [6] "isShowcase"       "isCertified"      "indexLevel"       "hasLayout"        "hasSample"       
    ## [11] "cxFileSize"       "cx2FileSize"      "visibility"       "nodeCount"        "edgeCount"       
    ## [16] "completed"        "version"          "owner"            "description"      "name"            
    ## [21] "properties"       "externalId"       "isDeleted"        "modificationTime" "creationTime"

``` r
networkSummary[c('name','externalId')]
```

    ## $name
    ## [1] "BNFO 286 (SP22) - WNT Signaling Pathway"
    ## 
    ## $externalId
    ## [1] "ff9c05f4-b502-11ec-b3be-0ac135e8bacf"

``` r
## get the entire network as RCX object
rcx <- ndex_get_network(ndexcon, networkId)
```

To send a network to an server, there are two possibilities. Either one
wants to update an existing network on the server or create a new one.
In both cases, a UUID is returned, either of the updated network or a
newly generated one for the created network. For updating a network, the
UUID is extracted from the “externalId” property of the “ndexStatus”
aspect, or can be set manually.

``` r
## create a new network on server
networkId <- ndex_create_network(ndexcon, rcx)

## update a network on server
networkId <- ndex_update_network(ndexcon, rcx)

## same as previous
networkId <- ndex_update_network(ndexcon, rcx, networkId)
```

Besides creating, reading and updating, it is also possible to delete
networks on the server. This operation cannot be undone, so be careful!

``` r
## deletes the network from the server
ndex_delete_network(ndexcon, networkId)
```

Example Workflow
================

This example workflow shows how to connect to the public NDEx server,
browse and retrieve the pathways of the Pathway Interaction Database of
the NCI which are hosted there.

``` r
## load the library!
library(ndexr)

## login to the NDEx server
ndexcon = ndex_connect()

## retrieve pathways of user "nci-pid"
networks_pid <- ndex_find_networks(ndexcon, accountName="nci-pid")

## list retrieved network information
networks_pid[,"name"]

## show information on the first pathways listed
networks_pid[1,]

## retrieve network data
mynetwork = ndex_get_network(ndexcon, networks_pid[1,"externalId"])

## convert into R graph format
mygraph = rcx_toRCXgraph(mynetwork)

## show graph information
mygraph

## use readable node names instead of IDs and plot the graph
V(mygraph)[as.character(mynetwork$nodes[,"@id"])]$name = mynetwork$nodes[,"n"]
plot(mygraph)
```

This code snippet starts with loading the ndexr library and connecting
to the server anonymously. Afterwards `ndex_find_networks` retrieves a
list of networks of user `nci-pid`, which contains the data of the
Pathway Interaction Database. The function `ndex_get_network` downloads
the network data and stores in the `RCX` format (explained in the next
section) and is then converted into an igraph-based object via
`rcx_toRCXgraph`. Here, the node IDs of the graph are set to readable
names and the graph is plotted. Naturally, this graph can be annotated
and beautified as required for the specific use cases.

RCX
===

For the exchange of network data, NDEx uses the Cytoscape
Cyberinfrastructure Network Interchange Format, or just CX format (See
[*http://www.home.ndexbio.org/data-model/*](http://www.home.ndexbio.org/data-model/)).
CX is an Aspect-Oriented Network Interchange Format encoded in JSON,
which is used as basis for the R implementation of the CX format, namely
RCX.

**Note: In future `ndexr` uses the `RCX` data model from the
corresponding package
(<a href="https://bioconductor.org/packages/RCX" class="uri">https://bioconductor.org/packages/RCX</a>)
to handle the networks!**

The RCX object is currently implemented within this package as a list of
data.frames, containing meta-data and all aspects of the network. The
structure of an RCX object, as shown via `str(rcx)` could be a list like
this:

``` r
str(rcx, max.level = 2)
```

    ## List of 10
    ##  $ nodes             :Classes 'NodesAspect' and 'data.frame':    31 obs. of  3 variables:
    ##   ..$ id        : int [1:31] 30 29 28 27 26 25 24 23 22 21 ...
    ##   ..$ name      : chr [1:31] "ROCK1" "CSNK1A1" "DKK1" "CTNNB1" ...
    ##   ..$ represents: chr [1:31] "uniprot:Q13464" "uniprot:P48729" "uniprot:O94907" "uniprot:P35222" ...
    ##  $ metaData          :Classes 'MetaDataAspect' and 'data.frame': 9 obs. of  5 variables:
    ##   ..$ name            : chr [1:9] "nodes" "edges" "nodeAttributes" "edgeAttributes" ...
    ##   ..$ version         : chr [1:9] "1.0" "1.0" "1.0" "1.0" ...
    ##   ..$ idCounter       : int [1:9] 30 71 NA NA NA NA NA NA NA
    ##   ..$ elementCount    : int [1:9] 31 72 62 603 4 31 3 3 35
    ##   ..$ consistencyGroup: num [1:9] 1 1 1 1 1 1 1 1 1
    ##  $ edges             :Classes 'EdgesAspect' and 'data.frame':    72 obs. of  4 variables:
    ##   ..$ id         : int [1:72] 68 53 36 32 31 30 27 71 70 33 ...
    ##   ..$ source     : int [1:72] 30 29 28 27 27 27 27 26 26 26 ...
    ##   ..$ target     : int [1:72] 26 27 3 7 7 7 8 5 5 3 ...
    ##   ..$ interaction: chr [1:72] "up-regulates activity" "down-regulates" "down-regulates" "up-regulates activity" ...
    ##  $ nodeAttributes    :Classes 'NodeAttributesAspect' and 'data.frame':   62 obs. of  5 variables:
    ##   ..$ propertyOf: int [1:62] 30 30 29 29 28 28 27 27 26 26 ...
    ##   ..$ name      : chr [1:62] "location" "type" "location" "type" ...
    ##   ..$ value     :List of 62
    ##   ..$ dataType  : chr [1:62] "string" "string" "string" "string" ...
    ##   ..$ isList    : logi [1:62] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ edgeAttributes    :Classes 'EdgeAttributesAspect' and 'data.frame':   603 obs. of  5 variables:
    ##   ..$ propertyOf: int [1:603] 68 68 68 68 68 68 68 68 53 53 ...
    ##   ..$ name      : chr [1:603] "sentence" "cell_data" "citation" "tissue_data" ...
    ##   ..$ value     :List of 603
    ##   ..$ dataType  : chr [1:603] "string" "string" "string" "string" ...
    ##   ..$ isList    : logi [1:603] FALSE TRUE TRUE TRUE FALSE FALSE ...
    ##  $ networkAttributes :Classes 'NetworkAttributesAspect' and 'data.frame':    4 obs. of  4 variables:
    ##   ..$ name    : chr [1:4] "name" "description" "version" "@context"
    ##   ..$ value   :List of 4
    ##   ..$ dataType: chr [1:4] "string" "string" "string" "string"
    ##   ..$ isList  : logi [1:4] FALSE FALSE FALSE FALSE
    ##  $ cartesianLayout   :Classes 'CartesianLayoutAspect' and 'data.frame':  31 obs. of  3 variables:
    ##   ..$ node: int [1:31] 8 16 24 1 9 17 25 2 10 18 ...
    ##   ..$ x   : num [1:31] 419 -181 -181 219 419 ...
    ##   ..$ y   : num [1:31] 195.1 395.1 -4.9 595.1 595.1 ...
    ##  $ cyVisualProperties:List of 3
    ##   ..$ network     :List of 5
    ##   .. ..- attr(*, "class")= chr [1:2] "CyVisualProperty" "list"
    ##   ..$ defaultNodes:List of 5
    ##   .. ..- attr(*, "class")= chr [1:2] "CyVisualProperty" "list"
    ##   ..$ defaultEdges:List of 5
    ##   .. ..- attr(*, "class")= chr [1:2] "CyVisualProperty" "list"
    ##   ..- attr(*, "class")= chr [1:2] "CyVisualPropertiesAspect" "list"
    ##  $ cyHiddenAttributes:Classes 'CyHiddenAttributesAspect' and 'data.frame':   3 obs. of  4 variables:
    ##   ..$ name    : chr [1:3] "NDEx UUID" "layoutAlgorithm" "NDEx Modification Timestamp"
    ##   ..$ value   :List of 3
    ##   ..$ dataType: chr [1:3] "string" "string" "string"
    ##   ..$ isList  : logi [1:3] FALSE FALSE FALSE
    ##  $ cyTableColumn     :Classes 'CyTableColumnAspect' and 'data.frame':    35 obs. of  4 variables:
    ##   ..$ appliesTo: chr [1:35] "nodes" "nodes" "nodes" "nodes" ...
    ##   ..$ name     : chr [1:35] "shared name" "name" "type" "location" ...
    ##   ..$ dataType : chr [1:35] "string" "string" "string" "string" ...
    ##   ..$ isList   : logi [1:35] FALSE FALSE FALSE FALSE TRUE FALSE ...
    ##  - attr(*, "class")= chr [1:2] "RCX" "list"

The data.frames representing nodes and edges could look like this:

``` r
rcx[["nodes"]][1:5,]
```

    ## Nodes:
    ##   id    name     represents
    ## 1 30   ROCK1 uniprot:Q13464
    ## 2 29 CSNK1A1 uniprot:P48729
    ## 3 28    DKK1 uniprot:O94907
    ## 4 27  CTNNB1 uniprot:P35222
    ## 5 26   MAPK8 uniprot:P45983

``` r
rcx[["edges"]][1:5,]
```

    ## Edges:
    ##   id source target           interaction
    ## 1 68     30     26 up-regulates activity
    ## 2 53     29     27        down-regulates
    ## 3 36     28      3        down-regulates
    ## 4 32     27      7 up-regulates activity
    ## 5 31     27      7 up-regulates activity

Usually, and RCX object is automatically created by using the functions
of this package for downloading network data from a NDEx server. But it
might be useful to convert an RCX object from/to JSON manually, for
example for down-/uploading a CX file from/to a NDEx server via the web
interface. For handling the network information within R, besides RCX
objects, one can use RCXgraph objects. A lossless conversion between the
two files can be done using the following functions:

``` r
## convert RCX to JSON
json <- RCX::toCX(rcx)

## ...and back
rcx <- RCX::parseJSON(json)

## convert RCX to RCXgraph
rcxgraph <- RCX::toIgraph(rcx)

## ...and back
rcx <- RCX::fromIgraph(rcxgraph)
```

It is possible to create blank RCX objects from scratch:

``` r
newRcx <- RCX::createRCX(RCX::createNodes())
```

After a RCX object is downloaded from an NDEx server, it will contain
some aspects that are not present in a newly generated network, i.e.
“ndexStatus”, “provenanceHistory” and “status”. Those aspects are
removed automatically by the package.

After a RCX object underwent some changes (adding/removing
nodes/edges/aspects/meta- data, etc.), it is advisable to check a RCX
object for its integrity and update its meta-data.

``` r
rcx <- RCX::updateMetaData(rcx)
```

The meta-data is not only updated by the function, but also created, if
not existent in the first place. It is necessary to mention, that the
function tries to check the format and content of most of the aspects
and properties, but due to the dynamic structure of RCX and CX, it is
advised to have a look at the CX data model specification for a deeper
insight about the core structure, dependencies and limitations.

Aspects and Metadata
====================

In general it is not advisable to retrieve a complete RCX object from a
server without knowing the number of aspects and its corresponding size,
because this may cause unwanted or unnecessary network traffic and
decline in performance. To avoid these problems, a possible workflow is
to download the meta-data of a network at first to check the available
aspects.

``` r
## get meta-data for a network
metadata = ndex_network_get_metadata(ndexcon, networkId)

names(metadata)
```

    ## [1] "name"         "elementCount" "version"      "idCounter"

``` r
metadata[c('name','elementCount')]
```

    ##                 name elementCount
    ## 1     nodeAttributes           62
    ## 2      cyTableColumn           35
    ## 3              edges           72
    ## 4 cyVisualProperties            3
    ## 5 cyHiddenAttributes            3
    ## 6              nodes           31
    ## 7  networkAttributes            4
    ## 8    cartesianLayout           31
    ## 9     edgeAttributes          603

Afterwards, only the favored aspects can be downloaded individually.

``` r
## get aspect "nodeCitations" for the network
networkAttibutes = ndex_network_get_aspect(ndexcon, networkId, "networkAttributes")

networkAttibutes
```

    ## Network attributes:
    ##          name
    ## 1        name
    ## 2 description
    ## 3     version
    ## 4    @context
    ##                                                                                                                                                                                                                                                                                                                                                                                                              value
    ## 1                                                                                                                                                                                                                                                                                                                                                                          BNFO 286 (SP22) - WNT Signaling Pathway
    ## 2                                                                                                                                                                                                                                                                                                                                                                                 Demo network for BNFO 286 (SP22)
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                  BNFO 286 - SP22
    ## 4 {"signor": "http://signor.uniroma2.it/relation_result.php?id=", "BTO": "http://identifiers.org/bto/BTO:", "uniprot": "http://identifiers.org/uniprot/", "pubmed": "http://identifiers.org/pubmed/", "CID": "http://identifiers.org/pubchem.compound/", "SID": "http://identifiers.org/pubchem.substance/", "chebi": "http://identifiers.org/chebi/CHEBI:", "hgnc.symbol": "http://identifiers.org/hgnc.symbol/"}
    ##   dataType isList
    ## 1   string  FALSE
    ## 2   string  FALSE
    ## 3   string  FALSE
    ## 4   string  FALSE

NDEx Network properties
=======================

Even after creation, it is possible to change the name, the description
or version of a network.

``` r
ndex_network_update_profile(ndexcon, networkId, name="My network", version="1.3")
ndex_network_update_profile(ndexcon, networkId, description="Nothing to see here")
```

For collaborative work, it is necessary to share networks between
several users and groups. Therefore there are specialized functions to
grant access to a network, change the permissions and withdraw access
permissions. It is possible to use those functions on single users or
groups. Possible permissions are “READ” to have reading access to
private networks, “WRITE” to be able modify, and “ADMIN” for the owner
of the network.

``` r
## show all user who have permission to a network
permissions = ndex_network_get_permission(ndexcon, networkId, 'user')

## show all groups who have permission to a network
permissions = ndex_network_get_permission(ndexcon, networkId, 'group')

## show all users with write access to a network
permissions = ndex_network_get_permission(ndexcon, networkId, 'user', 'WRITE')

## grant an user permission to a network
ndex_network_update_permission(ndexcon, networkId, user=someUserUuid, 'READ')

## change the permission of an user to the network
ndex_network_update_permission(ndexcon, networkId, user=someUserUuid, 'WRITE')

## withdraw the permission from an user
ndex_network_delete_permission(ndexcon, networkId, user=someUserUuid)
```

Besides permission management on user and group level, it is also
possible to set some system properties on a network that influence the
accessibility further. By default a network is private, which means that
it is only visible to the owner and invited users and groups. If at some
point one decides to make the network readable by anyone, it is possible
to change the visibility of a network to “PUBLIC”.

``` r
ndex_network_set_systemProperties(ndexcon, networkId, visibility="PUBLIC")
ndex_network_set_systemProperties(ndexcon, networkId, visibility="PRIVATE")
```

When a network has reached the point to be published, further edits
should be prevented. While it would be possible to set the access
permissions of all users and groups to “READ”, this approach is very
inconvenient. Therefore, a simpler way is to just set the network to
read-only using the network system properties.

``` r
ndex_network_set_systemProperties(ndexcon, networkId, readOnly=TRUE)
```

One also has the option at the NDEx server to choose a selection of
their favorite networks for display in his or her home page.

``` r
ndex_network_set_systemProperties(ndexcon, networkId, showcase=TRUE)
ndex_network_set_systemProperties(ndexcon, networkId, showcase=FALSE)
# change more than one property simultaneously
ndex_network_set_systemProperties(ndexcon, networkId, readOnly=TRUE, visibility="PUBLIC", showcase=TRUE)
```

The provenance history aspect of an NDEx network is used to document the
workflow of events and information sources that produced the current
network (for the official provenance documentation see
[*http://www.home.ndexbio.org/network-provenance-history/*](http://www.home.ndexbio.org/network-provenance-history/)
). There is a convenience function, that retrieves the provenance of the
network.

``` r
provenance = ndex_network_get_provenance(ndexcon, networkId) 
```

API Compatibility with NDEx versions
====================================

In the following table all API functions are listed. The functions are
grouped by the content they access, namely networks, users, or groups.
For every function also is shown, if authentication is needed, and by
which version it is supported (Version 2.0 or 1.3). A function marked
with brackets indicates, that, although the function would be supported
by this version, for different reasons no function could be implemented.
Limitations of the single API functions are also given in the column of
the corresponding version.

|                                       |                    |                 |                 |                                    |
|:--------------------------------------|:-------------------|:----------------|:----------------|:-----------------------------------|
| **Function name**                     | **Authentication** | **Version 2.1** | **Version 2.0** | **Version 1.3**                    |
| ***Networks***                        |                    |                 |                 |                                    |
| ndex\_find\_networks                  | no                 |                 | X               | X                                  |
| ndex\_network\_get\_summary           | no                 |                 | X               | X                                  |
| ndex\_get\_network                    | no                 |                 | X               | X                                  |
| ndex\_create\_network                 | yes                |                 | X               | X                                  |
| ndex\_update\_network                 | yes                |                 | X               | X                                  |
| ndex\_delete\_network                 | yes                |                 | X               | X                                  |
| ndex\_network\_get\_metadata          | no                 |                 | X               | (x)                                |
| ndex\_network\_aspect\_get\_metadata  | no                 |                 | (x)             |                                    |
| ndex\_network\_get\_aspect            | no                 |                 | X               | (x)                                |
| ndex\_network\_update\_aspect         | yes                |                 | (x)             |                                    |
| ndex\_network\_get\_permission        | yes                |                 | X               | only for users, different response |
| ndex\_network\_update\_permission     | yes                |                 | X               | (only for users)                   |
| ndex\_network\_delete\_permission     | yes                |                 | X               | only for users                     |
| ndex\_network\_set\_systemProperties  | yes                |                 | X               | only readOnly                      |
| ndex\_network\_update\_profile        | yes                |                 | X               | X                                  |
| ndex\_network\_get\_provenance        | no                 |                 | X               | X                                  |
| ***Users***                           |                    |                 |                 |                                    |
| ndex\_find\_users                     | no                 |                 | X               | X                                  |
| ndex\_find\_user\_byName              | no                 |                 | X               |                                    |
| ndex\_find\_user\_byId                | no                 |                 | X               |                                    |
| ndex\_create\_user                    | yes                |                 | X               |                                    |
| ndex\_delete\_user                    | yes                |                 | X               |                                    |
| ndex\_update\_user                    | yes                |                 | X               |                                    |
| ndex\_verify\_user                    | no                 |                 | X               |                                    |
| ndex\_user\_change\_password          | yes                |                 | X               |                                    |
| ndex\_user\_mail\_password            | no                 |                 | X               |                                    |
| ndex\_user\_forgot\_password          | no                 |                 | X               |                                    |
| ndex\_user\_list\_groups              | yes                |                 | X               |                                    |
| ndex\_user\_show\_group               | yes                |                 | X               |                                    |
| ndex\_user\_list\_permissions         | yes                |                 | X               |                                    |
| ndex\_user\_show\_permission          | yes                |                 | X               |                                    |
| ndex\_user\_get\_showcase             | no                 |                 | X               |                                    |
| ndex\_user\_get\_networksummary       | yes                |                 | X               |                                    |
| ***Groups***                          |                    |                 |                 |                                    |
| ndex\_find\_groups                    | no                 |                 | X               | X                                  |
| ndex\_get\_group                      | no                 |                 | X               |                                    |
| ndex\_create\_group                   | yes                |                 | X               |                                    |
| ndex\_delete\_group                   | yes                |                 | X               |                                    |
| ndex\_update\_group                   | yes                |                 | X               |                                    |
| ndex\_group\_list\_users              | no                 |                 | X               |                                    |
| ndex\_group\_set\_membership          | yes                |                 | X               |                                    |
| ndex\_group\_list\_networks           | no                 |                 | X               |                                    |
| ndex\_group\_network\_get\_permission | no                 |                 | X               |                                    |

Server REST API configuration
=============================

In the section “Connect to a server”, briefly a method for manually
changing the API version was introduced, with the API definition stored
in ndex\_config.

``` r
names(ndex_config)
```

    ## [1] "defaultVersion" "Version_2.1"    "Version_2.0"    "Version_1.3"

``` r
str(ndex_config, max.level = 3)
```

    ## List of 4
    ##  $ defaultVersion: chr "Version_2.1"
    ##  $ Version_2.1   :List of 3
    ##   ..$ version   : chr "2.1"
    ##   ..$ connection:List of 3
    ##   .. ..$ description: chr "URL of the NDEx server"
    ##   .. ..$ host       : chr "http://www.ndexbio.org"
    ##   .. ..$ api        : chr "/v2"
    ##   ..$ api       :List of 5
    ##   .. ..$ serverStatus:List of 4
    ##   .. ..$ user        :List of 11
    ##   .. ..$ group       :List of 6
    ##   .. ..$ network     :List of 12
    ##   .. ..$ search      :List of 3
    ##  $ Version_2.0   :List of 3
    ##   ..$ version   : chr "2.0"
    ##   ..$ connection:List of 3
    ##   .. ..$ description: chr "URL of the NDEx server"
    ##   .. ..$ host       : chr "http://www.ndexbio.org"
    ##   .. ..$ api        : chr "/v2"
    ##   ..$ api       :List of 5
    ##   .. ..$ serverStatus:List of 3
    ##   .. ..$ user        :List of 11
    ##   .. ..$ group       :List of 6
    ##   .. ..$ network     :List of 12
    ##   .. ..$ search      :List of 3
    ##  $ Version_1.3   :List of 3
    ##   ..$ version   : chr "1.3"
    ##   ..$ connection:List of 3
    ##   .. ..$ description: chr "URL of the NDEx server"
    ##   .. ..$ host       : chr "http://www.ndexbio.org"
    ##   .. ..$ api        : chr "/rest"
    ##   ..$ api       :List of 4
    ##   .. ..$ serverStatus:List of 3
    ##   .. ..$ user        :List of 1
    ##   .. ..$ network     :List of 11
    ##   .. ..$ search      :List of 3

This object contains the api definition for several versions (currently
version 1.3 and 2.0). By default, `ndex_connect()` uses the version
defined in `ndex_config$defaultVersion` (“Version\_2.0”). To use
another, or even a customized version to establish a server connection,
the ndexConf parameter can be used, like shown before. In the following,
the structure of such a configuration is elaborated in more detail.

``` r
names(ndex_config$Version_2.0)
```

    ## [1] "version"    "connection" "api"

The main structure of the configuration consists of three elements: The
version is used to distinguish manually set configurations, which is
used in some API functions to work differently for the single versions.

The “connection” element holds information about the default connection
to the server, namely the host (e.g.
“[*http://www.ndexbio.org/*](http://www.ndexbio.org/)”) and the path to
the api (e.g. “/v2” or “/rest”).

The REST API itself is defined in the “api” element, and follows the
scheme of the function names, or the scheme of url paths likewise. E.g.
in the “user” branch the different functions for handling user data are
defined. If required, the functions might be grouped together in
sub-branches furthermore. At the end of an branch, the actual definition
of an API can be found.

To show, how such an API is defined and how to define one by themselves,
let’s have a look at `ndex_config$Version_2.0$api$user$password$mail`,
which is used by `ndex_user_mail_password()`. Where to find the
configuration of the function is shown in the “REST query” section of
the function documentation. For a better readability, the yaml notation
for this function configuration is used:

``` r
mail:
  description: "Causes a new password to be generated for ..."
  url: "/user/#USERID#/password"
  method: "PUT"
  params:
    user:
      method: "replace"
      tag: "#USERID#"
    forgot:
      method: "parameter"
      tag: "forgot"
      default: "true"
```

Note: To get the yaml notation for the whole `ndex_config` simply use
`yaml::as.yaml(ndex_config)` (requires package `yaml` to be installed).

The single parameter definitions are given as list by the “params”
parameter. Each parameter is defined by a method, and, if applicable, a
tag, a default value and/or an optional flag. There are three keywords
defining the method: replace, append or parameter.

-   **replace:** The String defined by “tag” can be found within the url
    and will be replaced by the given value of the parameter. E.g. the
    tag `#NETWORKID#` in the url `/network/#NETWORKID#/provenance` is
    replaced by a value (e.g. `aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee`)
    given as network id, which leads to the url
    `/network/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/provenance`.
-   **append:** The given value of the parameter is appended to an url.
    Therefore the order of the parameters in the params definition is
    used. E.g. the url `/network/search` and the given values for
    `start = 0` and `size = 100` generates the following url:
    `/network/search/0/100`
-   **parameter:** Encodes the given parameters as url parameter using
    the specified tag as parameter descriptor. E.g. a parameter with the
    tag `username` and the value `SomeName` is encoded in the url
    `/user` as follows: `/user?username=SomeName`

It is also possible to set parameter as optional (except for replace),
or define default values. Values are assigned to the parameters using
the parameter name in the … parameter of the
`ndex_helper_encodeParams()` function. Parameters, that are not defined
in the configuration are ignored.

The easiest to write an own configuration is by simply copying an
existing configuration and tailor it to the needs.

``` r
# Copy an existing config
custom_ndex_config = ndex_config$Version_2.0

# Change the host connection for a local NDEx server installation
custom_ndex_config$connection$host ="localhost:8090"

# Custom path to the REST api
custom_ndex_config$connection$api ="/api/rest"

# Change the REST path for the ndex_get_network function
custom_ndex_config$api$network$get$url ="/custom/networks/#NETWORKID#"

# Add some (default) parameters to the function
custom_ndex_config$api$network$get$params$newParam = list(method="parameter", tag="someTag", default="someValue")
```

It is also possible to write an own configuration in yaml (or convert
`ndex_config` to yaml, see above) and load it as object (`yaml::load` or
`yaml::load_file`) or to translate the configuration into R code using
the function `ndexr:::yamlToRConfig()`

Note: For package maintenance it is advised to add new versions as yaml
definitions in `/R/ndex_api_config.yml` (for example as "Version\_3.0")
and update the R code in `/R/ndex_api_config.r`, which defines the
`ndex_config` object.

``` r
yamlToRConfig = function(yamlFile='R/ndex_api_config.yml', rScriptFile='R/ndex_api_config.r', defaultHeader=ndex_conf_header){
  yamlObj = yaml::yaml.load_file(yamlFile)
  rCodeTxt = paste0(defaultHeader, listToRCode(yamlObj))
  outFile = file(rScriptFile)
  writeLines(rCodeTxt, outFile)
  close(outFile)
}

yamlToRConfig()
```
