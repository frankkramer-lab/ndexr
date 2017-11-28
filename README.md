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
Elsevier Inc.
[ScienceDirect](http://www.sciencedirect.com/science/article/pii/S2405471215001477)).
NDEx can be used to upload, share and publicly distribute networks,
while providing an output in formats, that can be used by plenty of
other applications.

This package provides an interface to query the public NDEx server, as
well as private installations, in order to upload, download or modify
biological networks. This document aims to help the user to install and
benefit from the wide range of funtionality of this implementation. The
package also provides classes to implement the Cytoscape
Cyberinfrastructure (CX) Format and to extend the \[iGraph Package\]
(<http://igraph.org/r/>).

The package is compatible with both NDEx versions 1.3 and 2.0.

Installation
============

## Installation via Bioconductor

    source("https://bioconductor.org/biocLite.R")
    biocLite("ndexr")
    library(ndexr)
    
## Installation via GitHub
using [*devtools*](http://cran.r-project.org/web/packages/devtools/index.html) R package

    require(devtools)
    install_github("frankkramer-lab/ndexr")
    library(ndexr)

Quick Start
===========

Some short overview of the most important functions

    ## load the library!
    library(ndexr)

    ## login to the NDEx server
    ndexcon = ndex_connect("username", "password")

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

    ## do some fancy stuff with the network, then
    ## update the meta-data
    rcx <- rcx_updateMetaData(rcx)

    ## upload network as a new network to the NDEx server
    networkId <- ndex_create_network(ndexcon, rcx)

    ## do some other fancy stuff with the network, then
    ## update the network on the server
    networkId <- ndex_update_network(ndexcon, rcx)

    ## realize, you did bad things to the poor network, so better 
    ## delete it on the server
    ndex_delete_network(ndexcon, networkId)

Connect to a server
===================

First, establish an connection to the NDEx server. This object is
required for most of the other ndexr functions, because it stores
options and authentication details. It is possible to connect to the
server anonymously or provide a username and password to enable further
functionality. If you have set up your own NDEx server, you might change
the host to your local installation.

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
possible to restrict the networks to a specific search string (e.g.
“EGFR”), an account name (only networks of this account will be shown),
or limit the number of fetched networks.

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

As result you get a data.frame containing information of the networks.

    names(networks) 

    ##  [1] "ownerUUID"        "isReadOnly"       "subnetworkIds"   
    ##  [4] "errorMessage"     "isValid"          "warnings"        
    ##  [7] "isShowcase"       "visibility"       "edgeCount"       
    ## [10] "nodeCount"        "owner"            "uri"             
    ## [13] "version"          "description"      "name"            
    ## [16] "properties"       "externalId"       "isDeleted"       
    ## [19] "modificationTime" "creationTime"

    networks[,c('name','externalId')]

    ##                                                 name
    ## 1                               The Diabetes Machine
    ## 2                                    The RAS Machine
    ## 3                          conSCOM S. cerevisiae 152
    ## 4 Ravasi et al., CELL (2010) - Mouse TF interactions
    ## 5               Fanconi Anemia Gene Ontology (FangO)
    ##                             externalId
    ## 1 7aed4dd0-14e4-11e6-a1f8-06603eb7f303
    ## 2 50e3dff7-133e-11e6-a039-06603eb7f303
    ## 3 9a9d523b-f532-11e6-ab6b-0ac135e8bacf
    ## 4 4a5bc352-9243-11e5-b435-06603eb7f303
    ## 5 ab9bb6de-081a-11e7-aba2-0ac135e8bacf

Simple network operations
=========================

To both, users and networks stored on an NDEx server, a universally
unique identifier (UUID) is assigned. Although both have the same format
(i.e. “xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx”, where x is one of
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

    ## UUID of the first search result
    networkId <- networks[1,'externalId']

    ## get network summary
    networkSummary <- ndex_network_get_summary(ndexcon, networkId)

    names(networkSummary)

    ##  [1] "ownerUUID"        "isReadOnly"       "subnetworkIds"   
    ##  [4] "errorMessage"     "isValid"          "warnings"        
    ##  [7] "isShowcase"       "visibility"       "edgeCount"       
    ## [10] "nodeCount"        "owner"            "uri"             
    ## [13] "version"          "description"      "name"            
    ## [16] "properties"       "externalId"       "isDeleted"       
    ## [19] "modificationTime" "creationTime"

    networkSummary[c('name','externalId')]

    ## $name
    ## [1] "The Diabetes Machine"
    ## 
    ## $externalId
    ## [1] "7aed4dd0-14e4-11e6-a1f8-06603eb7f303"

    ## get the entire network as RCX object
    rcx <- ndex_get_network(ndexcon, networkId)

To send a network to an server, there are two possibilities. Either one
wants to update an existing network on the server or create a new one.
In both cases, a UUID is returned, either of the updated network or a
newly generated one for the created network. For updating a network, the
UUID is extracted from the “externalId” property of the “ndexStatus”
aspect, or can be set manually.

    ## create a new network on server
    networkId <- ndex_create_network(ndexcon, rcx)

    ## update a network on server
    networkId <- ndex_update_network(ndexcon, rcx)

    ## same as previous
    networkId <- ndex_update_network(ndexcon, rcx, networkId)

Besides creating, reading and updating, it is also possible to delete
networks on the server. This operation cannot be undone, so be careful!

    ## deletes the network from the server
    ndex_delete_network(ndexcon, networkId)

RCX
===

For the exchange of network data, NDEx uses the Cytoscape
Cyberinfrastructure Network Interchange Format, or just CX format (See
[*http://www.home.ndexbio.org/data-model/*](http://www.home.ndexbio.org/data-model/)).
CX is an Aspect-Oriented Network Interchange Format encoded in JSON,
which is used as basis for the R implementation of the CX format, namely
RCX.

The RCX object is currently implemented within this package as a list of
data.frames, containing meta-data and all aspects of the network. The
structure of an RCX object, as shown via `str(rcx)` could be a list like
this:

    str(rcx, max.level = 2)

    ## List of 13
    ##  $ metaData          :'data.frame':  11 obs. of  7 variables:
    ##   ..$ elementCount    : int [1:11] 1 1 395 667 328 299 3434 160 99 3 ...
    ##   ..$ lastUpdate      : num [1:11] 1.49e+12 1.49e+12 NA NA NA ...
    ##   ..$ name            : chr [1:11] "ndexStatus" "provenanceHistory" "nodes" "edges" ...
    ##   ..$ properties      :List of 11
    ##   ..$ version         : chr [1:11] "1.0" "1.0" NA NA ...
    ##   ..$ consistencyGroup: int [1:11] NA 1 NA NA NA NA NA NA NA NA ...
    ##   ..$ idCounter       : int [1:11] NA NA 1689 1689 1689 1689 NA NA NA NA ...
    ##  $ numberVerification:'data.frame':  1 obs. of  1 variable:
    ##   ..$ longNumber: num 2.81e+14
    ##  $ ndexStatus        :'data.frame':  1 obs. of  10 variables:
    ##   ..$ externalId      : chr "7aed4dd0-14e4-11e6-a1f8-06603eb7f303"
    ##   ..$ creationTime    : num 1.46e+12
    ##   ..$ modificationTime: num 1.49e+12
    ##   ..$ visibility      : chr "PUBLIC"
    ##   ..$ published       : logi FALSE
    ##   ..$ nodeCount       : int 395
    ##   ..$ edgeCount       : int 667
    ##   ..$ owner           : chr "rasmachine"
    ##   ..$ ndexServerURI   : chr "http://public.ndexbio.org"
    ##   ..$ readOnly        : logi FALSE
    ##  $ nodes             :'data.frame':  395 obs. of  2 variables:
    ##   ..$ @id: int [1:395] 0 1 5 10 11 20 27 28 32 33 ...
    ##   ..$ n  : chr [1:395] "ABCC8" "ATP" "CHI" "ADCY1" ...
    ##  $ edges             :'data.frame':  667 obs. of  4 variables:
    ##   ..$ @id: int [1:667] 2 6 12 21 29 34 38 43 53 58 ...
    ##   ..$ s  : int [1:667] 0 0 10 20 27 32 1 11 52 52 ...
    ##   ..$ t  : int [1:667] 1 5 11 11 28 33 10 42 10 57 ...
    ##   ..$ i  : chr [1:667] "Activation" "Activation" "Activation" "Activation" ...
    ##  $ supports          :'data.frame':  328 obs. of  4 variables:
    ##   ..$ text      : chr [1:328] "This validation of the model gives us confidence that robust predictions can be made for the role of gap junction coupling upon"| __truncated__ "Although treatment of CHI is primarily based on the use of diazoxide, CHI caused by recessive inactivating mutations in the ABC"| __truncated__ "Therefore, we report this case of CHI caused by a novel mutation of ABCC8 in a half-Korean newborn infant with diazoxide-unresp"| __truncated__ "Intracellular cAMP is synthesized by AC and can be degraded by phosphodiesterases (PDEs)." ...
    ##   ..$ citation  : logi [1:328] NA NA NA NA NA NA ...
    ##   ..$ @id       : int [1:328] 4 8 9 15 16 17 18 19 24 25 ...
    ##   ..$ attributes:List of 328
    ##   .. .. [list output truncated]
    ##  $ citations         :'data.frame':  299 obs. of  7 variables:
    ##   ..$ @id           : int [1:299] 3 7 13 14 22 23 30 35 39 44 ...
    ##   ..$ dc:title      : logi [1:299] NA NA NA NA NA NA ...
    ##   ..$ dc:contributor: logi [1:299] NA NA NA NA NA NA ...
    ##   ..$ dc:identifier : chr [1:299] "pmid:27681078" "pmid:28018462" "pmid:28261091" "pmid:27138453" ...
    ##   ..$ dc:type       : logi [1:299] NA NA NA NA NA NA ...
    ##   ..$ dc:description: logi [1:299] NA NA NA NA NA NA ...
    ##   ..$ attributes    :List of 299
    ##   .. .. [list output truncated]
    ##  $ edgeAttributes    :'data.frame':  3434 obs. of  3 variables:
    ##   ..$ po: int [1:3434] 2 2 2 2 2 2 6 6 6 6 ...
    ##   ..$ n : chr [1:3434] "INDRA statement" "type" "polarity" "Belief score" ...
    ##   ..$ v : chr [1:3434] "Activation(ABCC8(), ATP())" "Activation" "positive" "0.88" ...
    ##  $ edgeCitations     :'data.frame':  160 obs. of  2 variables:
    ##   ..$ po       :List of 160
    ##   .. .. [list output truncated]
    ##   ..$ citations:List of 160
    ##   .. .. [list output truncated]
    ##  $ edgeSupports      :'data.frame':  99 obs. of  2 variables:
    ##   ..$ po      :List of 99
    ##   ..$ supports:List of 99
    ##  $ networkAttributes :'data.frame':  3 obs. of  2 variables:
    ##   ..$ n: chr [1:3] "name" "description" "version"
    ##   ..$ v: chr [1:3] "The Diabetes Machine" "<div>The Diabetes Machine reads new publications on the molecular mechanisms behind drugs affecting insulin secretion in pancre"| __truncated__ "3.120"
    ##  $ nodeAttributes    :'data.frame':  1165 obs. of  3 variables:
    ##   ..$ po: int [1:1165] 0 0 0 1 1 1 1 5 5 10 ...
    ##   ..$ n : chr [1:1165] "type" "UniProt" "HGNC" "type" ...
    ##   ..$ v : chr [1:1165] "protein" "http://identifiers.org/uniprot/Q09428" "http://identifiers.org/hgnc/HGNC:59" "chemical" ...
    ##  $ status            :'data.frame':  1 obs. of  2 variables:
    ##   ..$ error  : chr ""
    ##   ..$ success: logi TRUE
    ##  - attr(*, "class")= chr [1:2] "RCX" "list"

The data.frames representing nodes and edges could look like this:

    rcx[["nodes"]][1:5,]

    ##   @id     n
    ## 1   0 ABCC8
    ## 2   1   ATP
    ## 3   5   CHI
    ## 4  10 ADCY1
    ## 5  11  CAMP

    rcx[["edges"]][1:5,]

    ##   @id  s  t          i
    ## 1   2  0  1 Activation
    ## 2   6  0  5 Activation
    ## 3  12 10 11 Activation
    ## 4  21 20 11 Activation
    ## 5  29 27 28 Activation

Usually, and RCX object is automatically created by using the functions
of this package for downloading network data from a NDEx server. But it
might be useful to convert an RCX object from/to JSON manually, for
example for down-/uploading a CX file from/to a NDEx server via the web
interface. For handling the network information within R, besides RCX
objects, one can use RCXgraph objects. A lossless conversion between the
two files can be done using the following functions:

    ## convert RCX to JSON
    json <- rcx_toJSON(rcx)

    ## ...and back
    rcx <- rcx_fromJSON(json)

    ## convert RCX to RCXgraph
    rcxgraph <- rcx_toRCXgraph(rcx)

    ## ...and back
    rcx <- rcxgraph_toRCX(rcxgraph)

It is possible to create blank RCX objects from scratch:

    newRcx <- rcx_new()

After a RCX object is downloaded from an NDEx server, it will contain
some aspects that are not present in a newly generated network, i.e.
“ndexStatus”, “provenanceHistory” and “status”. Removing those aspects
might be useful in some cases.

    asNewRcx <- rcx_asNewNetwork(rcx)

After a RCX object underwent some changes (adding/removing
nodes/edges/aspects/meta- data, etc.), it is advisable to check a RCX
object for its integrity and update its meta-data.

    rcx <- rcx_updateMetaData(rcx)

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

    ## get meta-data for a network
    metadata = ndex_network_get_metadata(ndexcon, networkId)

    names(metadata)

    ## [1] "consistencyGroup" "elementCount"     "lastUpdate"      
    ## [4] "name"             "properties"       "version"         
    ## [7] "idCounter"

    metadata[c('name','elementCount')]

    ##                 name elementCount
    ## 1  provenanceHistory            1
    ## 2              nodes          395
    ## 3              edges          667
    ## 4           supports          328
    ## 5          citations          299
    ## 6     edgeAttributes         3434
    ## 7      edgeCitations          160
    ## 8       edgeSupports           99
    ## 9  networkAttributes            3
    ## 10    nodeAttributes         1165

Afterwards, only the favored aspects can be downloaded individually.

    ## get aspect "nodeCitations" for the network
    networkAttibutes = ndex_network_get_aspect(ndexcon, networkId, "networkAttributes")

    networkAttibutes

    ##             n
    ## 1        name
    ## 2 description
    ## 3     version
    ##                                                                                                                                                                                                                      v
    ## 1                                                                                                                                                                                                 The Diabetes Machine
    ## 2 <div>The Diabetes Machine reads new publications on the molecular mechanisms behind drugs affecting insulin secretion in pancreatic beta cells automatically and assembles a model from them using INDRA.<br/></div>
    ## 3                                                                                                                                                                                                                3.120

NDEx Network properties
=======================

Even after creation, it is possible to change the name, the description
or version of a network.

    ndex_network_update_profile(ndexcon, networkId, name="My network", version="1.3")
    ndex_network_update_profile(ndexcon, networkId, description="Nothing to see here")

For collaborative work, it is necessary to share networks between
several users and groups. Therefore there are specialized functions to
grant access to a network, change the permissions and withdraw access
permissions. It is possible to use those functions on single users or
groups. Possible permissions are “READ” to have reading access to
private networks, “WRITE” to be able modify, and “ADMIN” for the owner
of the network.

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

Besides permission management on user and group level, it is also
possible to set some system properties on a network that influence the
accessibility further. By default a network is private, which means that
it is only visible to the owner and invited users and groups. If at some
point one decides to make the network readable by anyone, it is possible
to change the visibility of a network to “PUBLIC”.

    ndex_network_set_systemProperties(ndexcon, networkId, visibility="PUBLIC")
    ndex_network_set_systemProperties(ndexcon, networkId, visibility="PRIVATE")

When a network has reached the point to be published, further edits
should be prevented. While it would be possible to set the access
permissions of all users and groups to “READ”, this approach is very
inconvenient. Therefore, a simpler way is to just set the network to
read-only using the network system properties.

    ndex_network_set_systemProperties(ndexcon, networkId, readOnly=TRUE)

One also has the option at the NDEx server to choose a selection of
their favorite networks for display in his or her home page.

    ndex_network_set_systemProperties(ndexcon, networkId, showcase=TRUE)
    ndex_network_set_systemProperties(ndexcon, networkId, showcase=FALSE)
    # change more than one property simultaneously
    ndex_network_set_systemProperties(ndexcon, networkId, readOnly=TRUE, visibility="PUBLIC", showcase=TRUE)

The provenance history aspect of an NDEx network is used to document the
workflow of events and information sources that produced the current
network (for the official provenance documentation see
[*http://www.home.ndexbio.org/network-provenance-history/*](http://www.home.ndexbio.org/network-provenance-history/)
). There is a convenience function, that retrieves the provenance of the
network.

    provenance = ndex_network_get_provenance(ndexcon, networkId) 

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

<table>
<tbody>
<tr class="odd">
<td align="left"><strong>Function name</strong></td>
<td align="left"><strong>Authentication</strong></td>
<td align="left"><strong>Version 2.0</strong></td>
<td align="left"><strong>Version 1.3</strong></td>
</tr>
<tr class="even">
<td align="left"><strong><em>Networks</em></strong></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_find_networks</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left">X</td>
</tr>
<tr class="even">
<td align="left">ndex_network_get_summary</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left">X</td>
</tr>
<tr class="odd">
<td align="left">ndex_get_network</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left">X</td>
</tr>
<tr class="even">
<td align="left">ndex_create_network</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left">X</td>
</tr>
<tr class="odd">
<td align="left">ndex_update_network</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left">X</td>
</tr>
<tr class="even">
<td align="left">ndex_delete_network</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left">X</td>
</tr>
<tr class="odd">
<td align="left">ndex_network_get_metadata</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left">(x)</td>
</tr>
<tr class="even">
<td align="left">ndex_network_aspect_get_metadata</td>
<td align="left">no</td>
<td align="left">(x)</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_network_get_aspect</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left">(x)</td>
</tr>
<tr class="even">
<td align="left">ndex_network_update_aspect</td>
<td align="left">yes</td>
<td align="left">(x)</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_network_get_permission</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left">only for users, different response</td>
</tr>
<tr class="even">
<td align="left">ndex_network_update_permission</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left">(only for users)</td>
</tr>
<tr class="odd">
<td align="left">ndex_network_delete_permission</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left">only for users</td>
</tr>
<tr class="even">
<td align="left">ndex_network_set_systemProperties</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left">only readOnly</td>
</tr>
<tr class="odd">
<td align="left">ndex_network_update_profile</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left">X</td>
</tr>
<tr class="even">
<td align="left">ndex_network_get_provenance</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left">X</td>
</tr>
<tr class="odd">
<td align="left"><strong><em>Users</em></strong></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_find_users</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left">X</td>
</tr>
<tr class="odd">
<td align="left">ndex_find_user_byName</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_find_user_byId</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_create_user</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_delete_user</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_update_user</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_verify_user</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_user_change_password</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_user_mail_password</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_user_forgot_password</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_user_list_groups</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_user_show_group</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_user_list_permissions</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_user_show_permission</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_user_get_showcase</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_user_get_networksummary</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"><strong><em>Groups</em></strong></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_find_groups</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left">X</td>
</tr>
<tr class="even">
<td align="left">ndex_get_group</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_create_group</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_delete_group</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_update_group</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_group_list_users</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_group_set_membership</td>
<td align="left">yes</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">ndex_group_list_networks</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">ndex_group_network_get_permission</td>
<td align="left">no</td>
<td align="left">X</td>
<td align="left"></td>
</tr>
</tbody>
</table>

Server REST API configuration
=============================

In the section “Connect to a server”, briefly a method for manually
changing the API version was introduced, with the API definition stored
in ndex_config.

    names(ndex_config)

    ## [1] "defaultVersion" "Version_2.0"    "Version_1.3"

    str(ndex_config, max.level = 3)

    ## List of 3
    ##  $ defaultVersion: chr "Version_2.0"
    ##  $ Version_2.0   :List of 3
    ##   ..$ version   : chr "2.0"
    ##   ..$ connection:List of 3
    ##   .. ..$ description: chr "URL of the NDEx server"
    ##   .. ..$ host       : chr "www.ndexbio.org"
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
    ##   .. ..$ host       : chr "www.ndexbio.org"
    ##   .. ..$ api        : chr "/rest"
    ##   ..$ api       :List of 4
    ##   .. ..$ serverStatus:List of 3
    ##   .. ..$ user        :List of 1
    ##   .. ..$ network     :List of 11
    ##   .. ..$ search      :List of 3

This object contains the api definition for several versions (currently
version 1.3 and 2.0). By default, `ndex_connect()` uses the version
defined in `ndex_config$defaultVersion` ("Version\_2.0"). To use another,
or even a customized version to establish a server connection, the
ndexConf parameter can be used, like shown before. In the following, the
structure of such a configuration is elaborated in more detail.

    names(ndex_config$Version_2.0)

    ## [1] "version"    "connection" "api"

The main structure of the configuration consists of three elements: The
version is used to distinguish manually set configurations, which is
used in some API functions to work differently for the single versions.

The “connection” element holds information about the default connection
to the server, namely the host (e.g.
“[*http://www.ndexbio.org/*](http://www.ndexbio.org/)”) and the path to
the api (e.g. “/v2” or “/rest”).

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

Note: To get the yaml notation for the whole `ndex_config` simply use
`yaml::as.yaml(ndex_config)` (requires package `yaml` to be installed).

The single parameter definitions are given as list by the "params"
parameter. Each parameter is defined by a method, and, if applicable, a
tag, a default value and/or an optional flag. There are three keywords
defining the method: replace, append or parameter.

-   **replace:** The String defined by "tag" can be found within the url
    and will be replaced by the given value of the parameter. E.g. the
    tag `#NETWORKID#` in the url `/network/#NETWORKID#/provenance` is
    replaced by a value (e.g. `aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee`)
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
the parameter name in the ... parameter of the
`ndex_helper_encodeParams()` function. Parameters, that are not defined
in the configuration are ignored.

The easiest to write an own configuration is by simply copying an
existing configuration and tailor it to the needs.

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

It is also possible to write an own configuration in yaml (or convert
`ndex_config` to yaml, see above) and load it as object (`yaml::load` or
`yaml::load_file`) or to translate the configuration into R code using
the function `ndexr:::yamlToRConfig()`

Note: For package maintenance it is advised to add new versions as yaml
definitions in `/R/ndex_api_config.yml` (for example as “Version\_3.0”)
and update the R code in `/R/ndex_api_config.r`, which defines the
`ndex_config` object.

    yamlToRConfig = function(yamlFile='R/ndex_api_config.yml', rScriptFile='R/ndex_api_config.r', defaultHeader=ndex_conf_header){
      yamlObj = yaml::yaml.load_file(yamlFile)
      rCodeTxt = paste0(defaultHeader, listToRCode(yamlObj))
      outFile = file(rScriptFile)
      writeLines(rCodeTxt, outFile)
      close(outFile)
    }

    yamlToRConfig()
