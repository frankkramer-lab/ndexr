################################################################################
## Authors:
##   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##   Dexter Pratt [depratt@ucsd.edu]
##   Frank Kramer [frank.kramer@med.uni-goettingen.de]
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Split from ndex_networks on 25 January 2017 by Auer
##     
## Description:
##    Contains functions to search and retrieve networks
################################################################################


####################################################
## 
##   Aspects and Metadata
##
####################################################


#' Get Network CX Metadata Collection
#' 
#' This function retrieves the (aspect) meta-data of the network identified by the supplied network UUID string.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param networkId character; unique ID (UUID) of the network
#' 
#' @return metadata as list: consistencyGroup, elementCount, lastUpdate, name, properties, version and idCounter
#' 
#' @section REST query:
#' GET: ndex.api.config$api$network$aspect$getMetaData
#' @note Compatible to NDEx server version 1.3 and 2.0, but doesn't work for version 1.3
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' ndex.network.get.metadata(ndexcon,pws[1,"externalId"]) 
#' }
#' @export
ndex.network.get.metadata <- function(ndexcon, networkId){
    api = ndex.helper.getApi(ndexcon, 'network$aspect$getMetaData')
    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response$metaData)
}


#' Get the Metadata Associated with a Network UUID
#' 
#' This function retrieves the metadata associated with the supplied network UUID.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param networkId character; unique ID (UUID) of the network
#' @param aspect character; aspect name
#' 
#' @return metadata for an aspect as list: consistencyGroup, elementCount, lastUpdate, data, name, properties, version and idCounter
#' 
#' @section REST query:
#' GET: ndex.api.config$api$network$aspect$getMetaDataByName
#' @note Compatible to NDEx server version 2.0
#' @note Server error (version 2.0) since March 13th 2017
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' ndex.network.aspect.get.metadata(ndexcon,pws[1,"externalId"]) 
#' }
#' @export
ndex.network.aspect.get.metadata <- function(ndexcon, networkId, aspect){
    api = ndex.helper.getApi(ndexcon, 'network$aspect$getMetaDataByName')
    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId, aspect=aspect)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}

#' Get a Network Aspect As CX
#' 
#' This function retrieves the provided aspect as CX. The result is the same as accessing an aspect of a RCX object.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param networkId character; unique ID of the network
#' @param aspect character; name of the aspect
#' @param size integer; specifies the number of elements returned
#' @return data.frame of the aspect data (the same as rcx[[aspectName]])
#' @section REST query:
#' GET: ndex.api.config$api$network$aspect$getMetaDataByName
#' @note Compatible to NDEx server version 1.3 and 2.0, but doesn't work for version 1.3
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' aspect = ndex.network.get.aspect(ndexcon,pws[1,"externalId"], 'nodeAttributes')
#' aspect = ndex.network.get.aspect(ndexcon,pws[1,"externalId"], 'nodeAttributes', 10)    # limit the returned elemets of the aspect to the first 10 elements}
#' @export
ndex.network.get.aspect <- function(ndexcon, networkId, aspect, size){
    if(missing(size)) size = NULL
    
    api = ndex.helper.getApi(ndexcon, 'network$aspect$get')
    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId, aspect=aspect, size=size)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}

#' Update an Aspect of a Network
#' 
#' This function updates an aspect with the provided CX for the aspect.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param networkId unique ID of the network
#' @param aspectName name of the aspect
#' @param aspectAsRCX rcx data for the aspect (rcx[[aspectName]])
#' @return networkId unique ID of the modified network
#' @section REST query:
#' PUT: ndex.api.config$api$network$aspect$update
#' @note Compatible to NDEx server version 2.0
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' aspect = ndex.network.get.aspect(ndexcon,pws[1,"externalId"], 'nodeAttributes') 
#' aspectModified = aspect[1:5,]
#' ndex.network.update.aspect(ndexcon,pws[1,"externalId"], 'nodeAttributes', aspectModified)}
#' @export
ndex.network.update.aspect <- function(ndexcon, networkId, aspectName, aspectAsRCX){
# TODO!! : Error on server!
    api = ndex.helper.getApi(ndexcon, 'network$aspect$update')
    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId, aspect=aspectName)
    
    tmpFile = tempfile()
    writeLines(paste0('[{"',aspectName,'":[',rcx.aspect.toJSON(aspectAsRCX),']}]'), tmpFile)
    data <- list(CXNetworkStream = httr::upload_file(tmpFile, type = 'application/json'))
    response = ndex_rest_PUT(ndexcon, route, data, multipart=T, raw=T)
    file.remove(tmpFile)
    return(networkId)
}
