## ---- eval=FALSE-----------------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly=TRUE))
#      install.packages("BiocManager")
#  BiocManager::install("ndexr")
#  library(ndexr)

## ---- eval=FALSE-----------------------------------------------------------
#  require(devtools)
#  install_github("frankkramer-lab/ndexr")
#  library(ndexr)

## ---- results='hide', warning=FALSE, message=FALSE-------------------------
## load the library!
library(ndexr)

## ---- eval=FALSE-----------------------------------------------------------
#  ## login to the NDEx server
#  ndexcon = ndex_connect("username", "password")

## ---- echo=FALSE, results='hide', message=FALSE----------------------------
## login to the NDEx server
ndexcon = ndex_connect()

## --------------------------------------------------------------------------
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

## ---- eval=FALSE-----------------------------------------------------------
#  ## upload network as a new network to the NDEx server
#  networkId <- ndex_create_network(ndexcon, rcx)
#  
#  ## do some other fancy stuff with the network, then
#  ## update the network on the server
#  networkId <- ndex_update_network(ndexcon, rcx)
#  
#  ## realize, you did bad things to the poor network, so better
#  ## delete it on the server
#  ndex_delete_network(ndexcon, networkId)

## ---- eval=FALSE-----------------------------------------------------------
#  ## load the library
#  library(ndexr)
#  
#  ## connect anonymously
#  ndexcon = ndex_connect()
#  
#  ## log in with user name and password
#  ndexconUser = ndex_connect(username="username", password="password")
#  
#  ## specify the server
#  ndexconLocal = ndex_connect(username="username",
#  				password="password",
#  				host="localhost:8888/ndex/rest")
#  
#  ## manually change the api and connection configuration
#  ndexcon13 = ndex_connect(ndexConf=ndex_config$Version_1.3)

## ---- results = "hide"-----------------------------------------------------
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

## --------------------------------------------------------------------------
names(networks) 

networks[,c('name','externalId')]

## --------------------------------------------------------------------------
## UUID of the first search result
networkId <- networks[1,'externalId']

## get network summary
networkSummary <- ndex_network_get_summary(ndexcon, networkId)

names(networkSummary)

networkSummary[c('name','externalId')]

## get the entire network as RCX object
rcx <- ndex_get_network(ndexcon, networkId)

## ---- eval=FALSE-----------------------------------------------------------
#  ## create a new network on server
#  networkId <- ndex_create_network(ndexcon, rcx)
#  
#  ## update a network on server
#  networkId <- ndex_update_network(ndexcon, rcx)
#  
#  ## same as previous
#  networkId <- ndex_update_network(ndexcon, rcx, networkId)

## ---- eval=FALSE-----------------------------------------------------------
#  ## deletes the network from the server
#  ndex_delete_network(ndexcon, networkId)

## ---- eval=FALSE-----------------------------------------------------------
#  ## load the library!
#  library(ndexr)
#  
#  ## login to the NDEx server
#  ndexcon = ndex_connect()
#  
#  ## retrieve pathways of user "nci-pid"
#  networks_pid <- ndex_find_networks(ndexcon, accountName="nci-pid")
#  
#  ## list retrieved network information
#  networks_pid[,"name"]
#  
#  ## show information on the first pathways listed
#  networks_pid[1,]
#  
#  ## retrieve network data
#  mynetwork = ndex_get_network(ndexcon, networks_pid[1,"externalId"])
#  
#  ## convert into R graph format
#  mygraph = rcx_toRCXgraph(mynetwork)
#  
#  ## show graph information
#  mygraph
#  
#  ## use readable node names instead of IDs and plot the graph
#  V(mygraph)[as.character(mynetwork$nodes[,"@id"])]$name = mynetwork$nodes[,"n"]
#  plot(mygraph)

## --------------------------------------------------------------------------
str(rcx, max.level = 2)

## --------------------------------------------------------------------------
rcx[["nodes"]][1:5,]

rcx[["edges"]][1:5,]

## ---- warning=FALSE--------------------------------------------------------
## convert RCX to JSON
json <- rcx_toJSON(rcx)

## ...and back
rcx <- rcx_fromJSON(json)

## convert RCX to RCXgraph
rcxgraph <- rcx_toRCXgraph(rcx)

## ...and back
rcx <- rcxgraph_toRCX(rcxgraph)

## --------------------------------------------------------------------------
newRcx <- rcx_new()

## --------------------------------------------------------------------------
asNewRcx <- rcx_asNewNetwork(rcx)

## --------------------------------------------------------------------------
rcx <- rcx_updateMetaData(rcx)

## --------------------------------------------------------------------------
## get meta-data for a network
metadata = ndex_network_get_metadata(ndexcon, networkId)

names(metadata)

metadata[c('name','elementCount')]

## --------------------------------------------------------------------------
## get aspect "nodeCitations" for the network
networkAttibutes = ndex_network_get_aspect(ndexcon, networkId, "networkAttributes")

networkAttibutes

## ---- eval=FALSE-----------------------------------------------------------
#  ndex_network_update_profile(ndexcon, networkId, name="My network", version="1.3")
#  ndex_network_update_profile(ndexcon, networkId, description="Nothing to see here")

## ---- eval=FALSE-----------------------------------------------------------
#  ## show all user who have permission to a network
#  permissions = ndex_network_get_permission(ndexcon, networkId, 'user')
#  
#  ## show all groups who have permission to a network
#  permissions = ndex_network_get_permission(ndexcon, networkId, 'group')
#  
#  ## show all users with write access to a network
#  permissions = ndex_network_get_permission(ndexcon, networkId, 'user', 'WRITE')
#  
#  ## grant an user permission to a network
#  ndex_network_update_permission(ndexcon, networkId, user=someUserUuid, 'READ')
#  
#  ## change the permission of an user to the network
#  ndex_network_update_permission(ndexcon, networkId, user=someUserUuid, 'WRITE')
#  
#  ## withdraw the permission from an user
#  ndex_network_delete_permission(ndexcon, networkId, user=someUserUuid)

## ---- eval=FALSE-----------------------------------------------------------
#  ndex_network_set_systemProperties(ndexcon, networkId, visibility="PUBLIC")
#  ndex_network_set_systemProperties(ndexcon, networkId, visibility="PRIVATE")

## ---- eval=FALSE-----------------------------------------------------------
#  ndex_network_set_systemProperties(ndexcon, networkId, readOnly=TRUE)

## ---- eval=FALSE-----------------------------------------------------------
#  ndex_network_set_systemProperties(ndexcon, networkId, showcase=TRUE)
#  ndex_network_set_systemProperties(ndexcon, networkId, showcase=FALSE)
#  # change more than one property simultaneously
#  ndex_network_set_systemProperties(ndexcon, networkId, readOnly=TRUE, visibility="PUBLIC", showcase=TRUE)

## --------------------------------------------------------------------------
provenance = ndex_network_get_provenance(ndexcon, networkId) 

## --------------------------------------------------------------------------
names(ndex_config)
str(ndex_config, max.level = 3)

## --------------------------------------------------------------------------
names(ndex_config$Version_2.0)

## ---- eval=FALSE-----------------------------------------------------------
#  mail:
#    description: "Causes a new password to be generated for ..."
#    url: "/user/#USERID#/password"
#    method: "PUT"
#    params:
#      user:
#        method: "replace"
#        tag: "#USERID#"
#      forgot:
#        method: "parameter"
#        tag: "forgot"
#        default: "true"

## --------------------------------------------------------------------------
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

## ---- eval=FALSE-----------------------------------------------------------
#  yamlToRConfig = function(yamlFile='R/ndex_api_config.yml', rScriptFile='R/ndex_api_config.r', defaultHeader=ndex_conf_header){
#    yamlObj = yaml::yaml.load_file(yamlFile)
#    rCodeTxt = paste0(defaultHeader, listToRCode(yamlObj))
#    outFile = file(rScriptFile)
#    writeLines(rCodeTxt, outFile)
#    close(outFile)
#  }
#  
#  yamlToRConfig()

## --------------------------------------------------------------------------
sessionInfo()

