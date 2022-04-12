################################################################################
## Authors:
##   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
##   Dexter Pratt [depratt@ucsd.edu]
##   Frank Kramer [frank.kramer@informatik.uni-augsburg.de]
##   Florian Auer [florian.auer@informatik.uni-augsburg.de]
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
#' \strong{Note: In future `ndexr` uses the \link[RCX]{RCX-object} from the corresponding package to handle the networks!}
#' \strong{See also \link[RCX]{Meta-data} for more information.}
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param networkId character; unique ID (UUID) of the network
#' 
#' @return metadata as list: consistencyGroup, elementCount, lastUpdate, name, properties, version and idCounter
#' 
#' @section REST query:
#' GET: ndex_config$api$network$aspect$getMetaData
#' @note Compatible to NDEx server version 1.3 and 2.0, but doesn't work for version 1.3
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find a network and get its UUID
#' networks = ndex_find_networks(ndexcon,"p53", "nci-pid")
#' networkId = networks[1,"externalId"]
#' ## Get the network meta-data
#' ndex_network_get_metadata(ndexcon, networkId) 
#' @export
ndex_network_get_metadata <- function(ndexcon, networkId){
    api = ndex_helper_getApi(ndexcon, 'network$aspect$getMetaData')
    route <- ndex_helper_encodeParams(api$url, api$params, network=networkId)
    
    response = ndex_rest_GET(ndexcon, route)
    class(response) = c("MetaDataAspect", class(response))
    return(response$metaData)
}


#' Get the Metadata Associated with a Network UUID
#' 
#' This function retrieves the metadata associated with the supplied network UUID.
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param networkId character; unique ID (UUID) of the network
#' @param aspect character; aspect name
#' 
#' @return metadata for an aspect as list: consistencyGroup, elementCount, lastUpdate, data, name, properties, version and idCounter
#' 
#' @section REST query:
#' GET: ndex_config$api$network$aspect$getMetaDataByName
#' @note Compatible to NDEx server version 2.0
#' @note Server error (version 2.0) since March 13th 2017
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find a network and get its UUID
#' networks = ndex_find_networks(ndexcon,"p53", "nci-pid")
#' networkId = networks[1,"externalId"]
#' ## Get the meta-data of an aspect of a network
#' ndex_network_aspect_get_metadata(ndexcon, networkId, 'nodeAttributes') 
#' @export
ndex_network_aspect_get_metadata <- function(ndexcon, networkId, aspect){
    api = ndex_helper_getApi(ndexcon, 'network$aspect$getMetaDataByName')
    route <- ndex_helper_encodeParams(api$url, api$params, network=networkId, aspect=aspect)
    
    response = ndex_rest_GET(ndexcon, route)
    return(response)
}

#' Get a Network Aspect As CX
#' 
#' This function retrieves the provided aspect as CX. The result is the same as accessing an aspect of a RCX object.
#' 
#' \strong{Note: In future `ndexr` uses the \link[RCX]{RCX-object} from the corresponding package to handle the networks!}
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param networkId character; unique ID of the network
#' @param aspect character; name of the aspect
#' @param size integer; specifies the number of elements returned
#' 
#' @return data.frame of the aspect data (the same as rcx[[aspectName]])
#' 
#' @section REST query:
#' GET: ndex_config$api$network$aspect$getMetaDataByName
#' @note Compatible to NDEx server version 1.3 and 2.0, but doesn't work for version 1.3
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex_connect()
#' ## Find a network and get its UUID
#' networks = ndex_find_networks(ndexcon,"p53", "nci-pid")
#' networkId = networks[1,"externalId"]
#' ## Get the aspect of a network
#' aspect = ndex_network_get_aspect(ndexcon, networkId, 'nodeAttributes')
#' # limit the returned elements of the aspect to the first 10 elements
#' aspect = ndex_network_get_aspect(ndexcon, networkId, 'nodeAttributes', 10)
#' @export
ndex_network_get_aspect <- function(ndexcon, networkId, aspect, size){
    if(missing(size)) size = NULL
    
    api = ndex_helper_getApi(ndexcon, 'network$aspect$get')
    route <- ndex_helper_encodeParams(api$url, api$params, network=networkId, aspect=aspect, size=size)
    
    official_aspects = c(
        "nodes",
        "edges",
        "nodeAttributes",
        "edgeAttributes",
        "networkAttributes",
        "cartesianLayout",
        "cyGroups",
        "cyVisualProperties",
        "cyHiddenAttributes",
        "cyNetworkRelations",
        "cySubNetworks",
        "cyTableColumn"
    )
    
    if(aspect == "metaData"){
        response = ndex_network_get_metadata(ndexcon, networkId)
    }else if(aspect %in% official_aspects){
        raw_json = ndex_rest_GET(ndexcon, route, raw=TRUE)
        json = list()
        json[[aspect]] = jsonlite::fromJSON(raw_json, simplifyVector = FALSE)
        class(json) = c(aspect, class(json))
        response = RCX::jsonToRCX(json, verbose=FALSE)
    }else{
        response = ndex_rest_GET(ndexcon, route)
    }
    
    return(response)
}

#' Update an Aspect of a Network
#' 
#' This function updates an aspect with the provided CX for the aspect.
#' 
#' @param ndexcon object of class NDExConnection link{ndex_connect}
#' @param networkId unique ID of the network
#' @param aspectName name of the aspect
#' @param aspectAsRCX rcx data for the aspect (rcx[[aspectName]])
#' @param isJson logical if aspectAsRCX is already JSON
#' 
#' @return networkId unique ID of the modified network
#' @note Requires an authorized user! (ndex_connect with credentials)
#' 
#' @section REST query:
#' PUT: ndex_config$api$network$aspect$update
#' @note Compatible to NDEx server version 2.0
#' 
#' @examples 
#' ## Establish a server connection with credentials 
#' # ndexcon = ndex_connect('MyAccountName', 'MyPassword')
#' ## Find one of your networks and get its UUID
#' # networks = ndex_find_networks(ndexcon, accountName='MyAccountName')
#' # networkId = networks[1,"externalId"]
#' ## Get the network data 
#' # aspect = ndex_network_get_aspect(ndexcon, networkId, 'nodeAttributes') 
#' ## Do some changes to the aspect..
#' # aspectModified = aspect[1:5,]
#' ## and update the aspect
#' # ndex_network_update_aspect(ndexcon,pws[1,"externalId"], 'nodeAttributes', aspectModified)
#' NULL
#' @export
ndex_network_update_aspect <- function(ndexcon, networkId, aspectName, aspectAsRCX, isJson=FALSE){
# TODO!! : Error on server!
    api = ndex_helper_getApi(ndexcon, 'network$aspect$update')
    route <- ndex_helper_encodeParams(api$url, api$params, network=networkId)
    
    tmpFile = tempfile()
    if(isJson){
        # writeLines(paste0('[{"',aspectName,'":[',rcx_aspect_toJSON(aspectAsRCX),']}]'), tmpFile)
        writeLines(aspectAsRCX, tmpFile)
    }else{
        # writeLines(paste0('[',RCX::rcxToJson(aspectAsRCX, verbose=ndexcon$verbose),']'), tmpFile)
        writeLines(RCX::toCX(aspectAsRCX, verbose=ndexcon$verbose), tmpFile)
    }
    
    data <- list(CXNetworkStream = httr::upload_file(tmpFile, type = 'application/json'))
    response = ndex_rest_PUT(ndexcon, route, data, multipart=TRUE, raw=TRUE)
    file.remove(tmpFile)
    return(networkId)
}
