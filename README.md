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
range of functionality of this implementation. The package makes use of
the R implementation of the Cytoscape Cyberinfrastructure (CX) format by
the [RCX](https://doi.org/doi:10.18129/B9.bioc.RCX) package. The
[RCX](https://doi.org/doi:10.18129/B9.bioc.RCX) package provides
functions to create, edit, and extend the networks in CX format and also
for the lossless conversion of the networks from and to
[iGraph](http://igraph.org/r/) and [Bioconductor
graph](https://doi.org/doi:10.18129/B9.bioc.graph) objects.

The package is compatible with all NDEx API versions 1.3 and 2.x.

Installation
============

Installation from Bioconductor
------------------------------

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
BiocManager::install("ndexr")
library(ndexr)
```

Installation from GitHub
------------------------

using
[*devtools*](http://cran.r-project.org/web/packages/devtools/index.html)
R package

``` r
require(devtools)
install_github("frankkramer-lab/ndexr")
library(ndexr)
```

using
[remotes](https://cran.r-project.org/web/packages/remotes/index.html) R
package

``` r
require(remotes)
install_github("frankkramer-lab/ndexr")
library(ndexr)
```

Quick Start
===========

A short overview of the most important functions of the package:

``` r
## load the library!
library(ndexr)

## login to the NDEx server
ndexcon <- ndex_connect("username", "password")

## search the networks for 'EGFR'
networks <- ndex_find_networks(ndexcon, "EGFR")
head(networks, 3)
```

``` r
## UUID of the first search result
networkId <- networks[1, "externalId"]
networkId
```

    ## [1] "f71ab602-97f0-11eb-9e72-0ac135e8bacf"

``` r
## get summary of the network
ndex_network_get_summary(ndexcon, networkId)
```

    ## $ownerUUID
    ## [1] "0db1f2dc-103f-11e8-b939-0ac135e8bacf"
    ## 
    ## $isReadOnly
    ## [1] FALSE
    ## 
    ## $subnetworkIds
    ## list()
    ## 
    ## $isValid
    ## [1] TRUE
    ## 
    ## $warnings
    ## list()
    ## 
    ## $isShowcase
    ## [1] FALSE
    ## 
    ## $isCertified
    ## [1] FALSE
    ## 
    ## $indexLevel
    ## [1] "ALL"
    ## 
    ## $hasLayout
    ## [1] TRUE
    ## 
    ## $hasSample
    ## [1] FALSE
    ## 
    ## $cxFileSize
    ## [1] 93354
    ## 
    ## $cx2FileSize
    ## [1] 70786
    ## 
    ## $visibility
    ## [1] "PUBLIC"
    ## 
    ## $nodeCount
    ## [1] 32
    ## 
    ## $edgeCount
    ## [1] 61
    ## 
    ## $version
    ## [1] "02-Sep-2022"
    ## 
    ## $owner
    ## [1] "signor"
    ## 
    ## $completed
    ## [1] TRUE
    ## 
    ## $description
    ## [1] "The epidermal growth factor receptor (EGFR) signaling pathway regulates growth, survival, proliferation, and differentiation. The binding of extracellular ligands (EGF) induces homo and heterodimerization, transphosphorylation and activation of four ErbB family receptors: EGFR (ErbB1), ErbB2, ErbB3, and ErbB4. These events trigger a cascade of activation of downstream pathways that include, principally, the MAPK, Akt and JNK pathways, culminating in DNA synthesis and cell proliferation."
    ## 
    ## $name
    ## [1] "EGFR Signaling"
    ## 
    ## $properties
    ##    subNetworkId        predicateString       dataType
    ## 1            NA               @context         string
    ## 2            NA                 labels list_of_string
    ## 3            NA                 author         string
    ## 4            NA               organism         string
    ## 5            NA           rightsHolder         string
    ## 6            NA                 rights         string
    ## 7            NA              reference         string
    ## 8            NA            networkType list_of_string
    ## 9            NA    prov:wasGeneratedBy         string
    ## 10           NA __normalizationversion         string
    ## 11           NA    prov:wasDerivedFrom         string
    ## 12           NA                  notes         string
    ##                                                                                                                                                                                                                                                                                                                                                                                                               value
    ## 1  {"signor": "http://signor.uniroma2.it/relation_result.php?id=", "BTO": "http://identifiers.org/bto/BTO:", "uniprot": "http://identifiers.org/uniprot/", "pubmed": "http://identifiers.org/pubmed/", "CID": "http://identifiers.org/pubchem.compound/", "SID": "http://identifiers.org/pubchem.substance/", "chebi": "http://identifiers.org/chebi/CHEBI:", "hgnc.symbol": "http://identifiers.org/hgnc.symbol/"}
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                    ["SIGNOR-EGF"]
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                 Theodora Pavlidou
    ## 4                                                                                                                                                                                                                                                                                                                                                                                              Homo Sapiens (human)
    ## 5                                                                                                                                                                                                                                                                                                                                                                                             Prof. Gianni Cesareni
    ## 6                                                                                                                                                                                                                                                                                                                                                        Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
    ## 7                                                                                                    <div>Perfetto L., <i>et al.</i></div><div><b>SIGNOR: a database of causal relationships between biological entities</b><i>.</i></div><div>Nucleic Acids Res. 2016 Jan 4;44(D1):D548-54</div><div><span><a href="https://doi.org/10.1093/nar/gkv1048" target="_blank">doi: 10.1093/nar/gkv1048</a></span></div>
    ## 8                                                                                                                                                                                                                                                                                                                                                                                  ["pathway","Signalling Pathway"]
    ## 9                                                                                                                                                                                                                                                                                                                              <a href="https://github.com/ndexcontent/ndexsignorloader">ndexsignorloader 1.2.0</a>
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                              0.1
    ## 11                                                                                                                                                                                                                                                                                                                                 https://signor.uniroma2.it/pathway_browser.php?organism=&pathway_list=SIGNOR-EGF
    ## 12                                                                                                                                                                                                                                                                                                                  Edges have been collapsed with attributes converted to lists with exception of direct attribute
    ## 
    ## $externalId
    ## [1] "f71ab602-97f0-11eb-9e72-0ac135e8bacf"
    ## 
    ## $isDeleted
    ## [1] FALSE
    ## 
    ## $modificationTime
    ## [1] 1.663185e+12
    ## 
    ## $creationTime
    ## [1] 1.617835e+12

``` r
## get the entire network as RCX object
rcx <- ndex_get_network(ndexcon, networkId)

## show the content (aspects) of the network
rcx$metaData
```

    ## Meta-data:

``` r
## upload network as a new network to the NDEx server
networkId <- ndex_create_network(ndexcon, rcx)

## do some other fancy stuff with the network, then update the
## network on the server
networkId <- ndex_update_network(ndexcon, rcx)

## realize, you did bad things to the poor network, so better delete
## it on the server
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
ndexcon <- ndex_connect()

## log in with user name and password
ndexconUser <- ndex_connect(username="username", password="password")

## specify the server
ndexconLocal <- ndex_connect(
  username="username",
  password="password", 
  host="localhost:8888/ndex/rest"
)

## manually change the api and connection configuration
ndexcon13 <- ndex_connect(ndexConf=ndex_config$Version_1.3)
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
a function to retrieve a list of networks from the server.

``` r
## list networks on server
networks <- ndex_find_networks(ndexcon)
```

As result you get a data.frame containing information of the networks.

``` r
names(networks)
```

    ##  [1] "ownerUUID"        "isReadOnly"       "subnetworkIds"    "isValid"          "warnings"         "isShowcase"       "isCertified"     
    ##  [8] "indexLevel"       "hasLayout"        "hasSample"        "cxFileSize"       "cx2FileSize"      "visibility"       "nodeCount"       
    ## [15] "edgeCount"        "version"          "owner"            "completed"        "description"      "name"             "properties"      
    ## [22] "externalId"       "isDeleted"        "modificationTime" "creationTime"     "doi"

``` r
networks[1:5, c("name", "externalId")]
```

It is possible to restrict the networks to a specific search string
(e.g. “EGFR”), an account name (only networks of this account will be
shown), or limit the number of fetched networks.

``` r
## list networks on server (same as previous)
networks <- ndex_find_networks(ndexcon, start = 0, size = 5)

## search for 'EGFR'
networksEgfr <- ndex_find_networks(ndexcon, searchString = "EGFR")
## same as previous
networksEgfr <- ndex_find_networks(ndexcon, "EGFR")
networksEgfr[1:3, ]

## search for networks of a user
networksOfUser <- ndex_find_networks(ndexcon, accountName = "ndextutorials")
networksOfUser[1:5, c("name", "owner", "externalId")]
```

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
networkId <- networksOfUser[1, "externalId"]

## get network summary
networkSummary <- ndex_network_get_summary(ndexcon, networkId)

names(networkSummary)
```

    ##  [1] "ownerUUID"        "isReadOnly"       "subnetworkIds"    "isValid"          "warnings"         "isShowcase"       "isCertified"     
    ##  [8] "indexLevel"       "hasLayout"        "hasSample"        "cxFileSize"       "cx2FileSize"      "visibility"       "nodeCount"       
    ## [15] "edgeCount"        "version"          "owner"            "completed"        "description"      "name"             "properties"      
    ## [22] "externalId"       "isDeleted"        "modificationTime" "creationTime"

``` r
networkSummary[c("name", "externalId")]
```

    ## $name
    ## [1] "BNFO 286 (SP22) - WNT Signaling Pathway"
    ## 
    ## $externalId
    ## [1] "ff9c05f4-b502-11ec-b3be-0ac135e8bacf"

``` r
## get the entire network as RCX object
rcx <- ndex_get_network(ndexcon, networkId)
rcx$metaData
```

    ## Meta-data:

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

RCX
===

For the exchange of network data, NDEx uses the Cytoscape
Cyberinfrastructure Network Interchange Format, or just CX format (See
[*http://www.home.ndexbio.org/data-model/*](http://www.home.ndexbio.org/data-model/)).
CX is an Aspect-Oriented Network Interchange Format encoded in JSON,
which is used as basis for the R implementation of the CX format, namely
RCX. The `RCX` data model is implemented in the corresponding
[RCX](https://doi.org/doi:10.18129/B9.bioc.RCX) to handle the networks.

Example Workflow
================

This example workflow shows how to connect to the public NDEx server,
browse and retrieve the pathways of the Pathway Interaction Database of
the NCI which are hosted there.

``` r
## load the library!
library(ndexr)

## login to the NDEx server
ndexcon <- ndex_connect()

## retrieve pathways of user 'nci-pid'
networks_pid <- ndex_find_networks(ndexcon, accountName = "nci-pid")

## list retrieved network information (only the first 10 entries)
networks_pid[1:10, "name"]
```

    ##  [1] "ErbB2ErbB3 signaling events (v2.0)"                   "TCR signaling in nave CD4 T cells (v2.0)"            
    ##  [3] "ErbB4 signaling events (v2.0)"                        "Fanconi anemia pathway (v2.0)"                       
    ##  [5] "TCR signaling in nave CD8 T cells (v2.0)"             "FAS (CD95) signaling pathway (v2.0)"                 
    ##  [7] "Fc-epsilon receptor I signaling in mast cells (v2.0)" "TGF-beta receptor signaling (v2.0)"                  
    ##  [9] "FGF signaling pathway (v2.0)"                         "Thromboxane A2 receptor signaling (v2.0)"

``` r
## show information on the first pathways listed
networks_pid[1, ]
```

``` r
## retrieve network data
mynetwork <- ndex_get_network(ndexcon, networks_pid[1, "externalId"])

## visualize the network with RCX
RCX::visualize(mynetwork)
```

![](https://github.com/frankkramer-lab/ndexr/blob/master/vignettes/EGFR_Signaling.png)

This code snippet starts with loading the ndexr library and connecting
to the server anonymously. Afterwards `ndex_find_networks` retrieves a
list of networks of user `nci-pid`, which contains the data of the
Pathway Interaction Database. The function `ndex_get_network` downloads
the network data and stores in the `RCX` format and is then converted
into an igraph object via `RCX::toIgraph`. Here, the node IDs of the
graph are set to readable names and the graph is plotted. Naturally,
this graph can be annotated and beautified as required for the specific
use cases.

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
metadata <- ndex_network_get_metadata(ndexcon, networkId)

names(metadata)
```

    ## [1] "name"         "elementCount" "version"      "idCounter"

``` r
metadata[c("name", "elementCount")]
```

Afterwards, only the favored aspects can be downloaded individually.

``` r
## get aspect 'nodeCitations' for the network
networkAttibutes <- ndex_network_get_aspect(ndexcon, networkId, "networkAttributes")

networkAttibutes
```

    ## Network attributes:

NDEx Network properties
=======================

Even after creation, it is possible to change the name, the description
or version of a network.

``` r
ndex_network_update_profile(ndexcon, networkId, name = "My network", version = "1.3")
ndex_network_update_profile(ndexcon, networkId, description = "Nothing to see here")
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
ndex_network_get_permission(ndexcon, networkId, "user")

## show all groups who have permission to a network
ndex_network_get_permission(ndexcon, networkId, "group")

## show all users with write access to a network
ndex_network_get_permission(ndexcon, networkId, "user", "WRITE")

## grant an user permission to a network
ndex_network_update_permission(ndexcon, networkId, user = someUserUuid, "READ")

## change the permission of an user to the network
ndex_network_update_permission(ndexcon, networkId, user = someUserUuid, "WRITE")

## withdraw the permission from an user
ndex_network_delete_permission(ndexcon, networkId, user = someUserUuid)
```

Besides permission management on user and group level, it is also
possible to set some system properties on a network that influence the
accessibility further. By default a network is private, which means that
it is only visible to the owner and invited users and groups. If at some
point one decides to make the network readable by anyone, it is possible
to change the visibility of a network to “PUBLIC”.

``` r
ndex_network_set_systemProperties(ndexcon, networkId, visibility = "PUBLIC")
ndex_network_set_systemProperties(ndexcon, networkId, visibility = "PRIVATE")
```

When a network has reached the point to be published, further edits
should be prevented. While it would be possible to set the access
permissions of all users and groups to “READ”, this approach is very
inconvenient. Therefore, a simpler way is to just set the network to
read-only using the network system properties.

``` r
ndex_network_set_systemProperties(ndexcon, networkId, readOnly = TRUE)
```

One also has the option at the NDEx server to choose a selection of
their favorite networks for display in his or her home page.

``` r
ndex_network_set_systemProperties(ndexcon, networkId, showcase = TRUE)
ndex_network_set_systemProperties(ndexcon, networkId, showcase = FALSE)
# change more than one property simultaneously
ndex_network_set_systemProperties(
    ndexcon, networkId, readOnly = TRUE, visibility = "PUBLIC", showcase = TRUE
)
```

**The provenance history aspect is now deprecated within the CX
specification! The following description is left here for completeness
and compatibility with old network specification!**

The provenance history aspect of an NDEx network is used to document the
workflow of events and information sources that produced the current
network (for the official provenance documentation see
[*http://www.home.ndexbio.org/network-provenance-history/*](http://www.home.ndexbio.org/network-provenance-history/)
). There is a convenience function, that retrieves the provenance of the
network.

``` r
provenance <- ndex_network_get_provenance(ndexcon, networkId)
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

|                                       |                    |                 |                                    |
|:--------------------------------------|:-------------------|:----------------|:-----------------------------------|
| **Function name**                     | **Authentication** | **Version 2.x** | **Version 1.3**                    |
| ***Networks***                        |                    |                 |                                    |
| ndex\_find\_networks                  | no                 | X               | X                                  |
| ndex\_network\_get\_summary           | no                 | X               | X                                  |
| ndex\_get\_network                    | no                 | X               | X                                  |
| ndex\_create\_network                 | yes                | X               | X                                  |
| ndex\_update\_network                 | yes                | X               | X                                  |
| ndex\_delete\_network                 | yes                | X               | X                                  |
| ndex\_network\_get\_metadata          | no                 | X               | (x)                                |
| ndex\_network\_aspect\_get\_metadata  | no                 | (x)             |                                    |
| ndex\_network\_get\_aspect            | no                 | X               | (x)                                |
| ndex\_network\_update\_aspect         | yes                | (x)             |                                    |
| ndex\_network\_get\_permission        | yes                | X               | only for users, different response |
| ndex\_network\_update\_permission     | yes                | X               | (only for users)                   |
| ndex\_network\_delete\_permission     | yes                | X               | only for users                     |
| ndex\_network\_set\_systemProperties  | yes                | X               | only readOnly                      |
| ndex\_network\_update\_profile        | yes                | X               | X                                  |
| ndex\_network\_get\_provenance        | no                 | (x)             | X                                  |
| ***Users***                           |                    |                 |                                    |
| ndex\_find\_users                     | no                 | X               | X                                  |
| ndex\_find\_user\_byName              | no                 | X               |                                    |
| ndex\_find\_user\_byId                | no                 | X               |                                    |
| ndex\_create\_user                    | yes                | X               |                                    |
| ndex\_delete\_user                    | yes                | X               |                                    |
| ndex\_update\_user                    | yes                | X               |                                    |
| ndex\_verify\_user                    | no                 | X               |                                    |
| ndex\_user\_change\_password          | yes                | X               |                                    |
| ndex\_user\_mail\_password            | no                 | X               |                                    |
| ndex\_user\_forgot\_password          | no                 | X               |                                    |
| ndex\_user\_list\_groups              | yes                | X               |                                    |
| ndex\_user\_show\_group               | yes                | X               |                                    |
| ndex\_user\_list\_permissions         | yes                | X               |                                    |
| ndex\_user\_show\_permission          | yes                | X               |                                    |
| ndex\_user\_get\_showcase             | no                 | X               |                                    |
| ndex\_user\_get\_networksummary       | yes                | X               |                                    |
| ***Groups***                          |                    |                 |                                    |
| ndex\_find\_groups                    | no                 | X               | X                                  |
| ndex\_get\_group                      | no                 | X               |                                    |
| ndex\_create\_group                   | yes                | X               |                                    |
| ndex\_delete\_group                   | yes                | X               |                                    |
| ndex\_update\_group                   | yes                | X               |                                    |
| ndex\_group\_list\_users              | no                 | X               |                                    |
| ndex\_group\_set\_membership          | yes                | X               |                                    |
| ndex\_group\_list\_networks           | no                 | X               |                                    |
| ndex\_group\_network\_get\_permission | no                 | X               |                                    |

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

``` yaml
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
custom_ndex_config <- ndex_config$Version_2.0

# Change the host connection for a local NDEx server installation
custom_ndex_config$connection$host <- "localhost:8090"

# Custom path to the REST api
custom_ndex_config$connection$api <- "/api/rest"

# Change the REST path for the ndex_get_network function
custom_ndex_config$api$network$get$url <- "/custom/networks/#NETWORKID#"

# Add some (default) parameters to the function
custom_ndex_config$api$network$get$params$newParam <- list(method = "parameter", tag = "someTag", default = "someValue")
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
yamlToRConfig <- function(
    yamlFile = "R/ndex_api_config.yml", rScriptFile = "R/ndex_api_config.r",
    defaultHeader = ndex_conf_header
) {
    yamlObj <- yaml::yaml.load_file(yamlFile)
    rCodeTxt <- paste0(defaultHeader, listToRCode(yamlObj))
    outFile <- file(rScriptFile)
    writeLines(rCodeTxt, outFile)
    close(outFile)
}

yamlToRConfig()
```
