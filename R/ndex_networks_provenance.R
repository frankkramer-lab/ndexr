################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Split from ndex_networks_ndex_properties on 23.03.2017 by Auer
##     
## Description:
##   Functions for generating and changing the network provenance
################################################################################

####################################################
## Network Provenance
## For structure and documentation see:  http://www.home.ndexbio.org/network-provenance-history/
####################################################


#' Get Network Provenance
#' 
#' This function retrieves the provenance of the network identified by the supplied network UUID string.
#' 
#' @param ndexcon object of class NDEXConnection link{ndex.connect}
#' @param networkId unique ID of the network
#' 
#' @return List of network metadata: ID, name, whether it is public, edge and node count; source and format of network
#' 
#' @section REST query:
#' GET: ndex.conf$api$network$provenance$get
#' @note Compatible to NDEx server version 1.3 and 2.0
#' 
#' @examples 
#' ## Establish a server connection
#' ndexcon = ndex.connect()
#' ## Find a network and get its UUID
#' networks = ndex.find.networks(ndexcon,"p53")
#' networkId = networks[1,"externalId"]
#' ## Get the network provenace
#' provenance = ndex.network.get.provenance(ndexcon, networkId) 
#' @export
ndex.network.get.provenance <- function(ndexcon, networkId){
    
    api = ndex.helper.getApi(ndexcon, 'network$provenance$get')
    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId)
    
    response <- ndex_rest_GET(ndexcon, route)
    return(response)
}



####################################################
##   Experimental and unfinished
####################################################

##' Set Network Provenance    
##' 
##' Updates the 'provenance' field of the network specified by 'networkId' to be the ProvenanceEntity object in the PUT data. The ProvenanceEntity object is expected to represent the current state of the network and to contain a tree-structure of ProvenanceEvent and ProvenanceEntity objects that describe the networks provenance history.
##' 
##' @param ndexcon object of class NDEXConnection link{ndex.connect}
##' @param networkId character; unique ID (UUID) of the network
##' @param provenance NDEx provanance object
##' 
##' @return List of network metadata: ID, name, whether it is public, edge and node count; source and format of network
##' 
##' @section REST query:
##' GET: ndex.conf$api$network$provenance$get
##' @note Compatible to NDEx server version 1.3 and 2.0
##' 
##' @examples 
##' \dontrun{
##' ndexcon = ndex.connect('MyAccountName', 'MyPassword', verbose=T)
##' networks = ndex.find.networks(ndexcon,"p53")
##' networkId = networks[1,"externalId"]
##' provenance = ndex.network.set.provenance(ndexcon, networkId, provenance) 
##' }
##' NULL
#ndex.network.set.provenance <- function(ndexcon, networkId, provenance){    
## TODO! : Implement! Needs some way to construct ProvenanceEntity and ProvenanceEvent objects (see http://www.home.ndexbio.org/network-provenance-history/ ). How to encode merge in JSON/R?
#    
#    api = ndex.helper.getApi(ndexcon, 'network$provenance$set')
#    route <- ndex.helper.encodeParams(api$url, api$params, network=networkId)
#    
#    tmpFile = tempfile()
#    writeLines(jsonlite::toJSON(provenance), tmpFile)
#    data <- list(CXNetworkStream = httr::upload_file(tmpFile, type = 'application/json'))
#    response = ndex_rest_PUT(ndexcon, route, data, multipart=T, raw=T)
#    file.remove(tmpFile)
#    return(NULL)
#}
#
#
##' Create Network Provenance Event
##'
##' @param inputs array of ProvenanceEntity objects - Has semantics of PROV:used
##' @param startedAtTime timestamp - Has semantics of PROV:startingAtTime
##' @param endedAtTime timestamp - Has semantics of PROV:endingAtTime
##' @param eventType name or description of the event (e.g. "File Upload")
##' @param property.name array of names for name-value pairs
##' @param property.value array of values for name-value pairs
##' 
##' @return Network ProvenanceEvent Object
##' @examples 
##' \dontrun{}
##' NULL
#ndex.create.provenance.event <- function(inputs=NA, startedAtTime, endedAtTime, eventType, property.name, property.value){
#    event = data.frame(inputs=inputs, startedAtTime=startedAtTime, endedAtTime=endedAtTime, eventType=eventType, properties=NA)
#    event$properties = as.list(event$properties)
#    event$properties = list(data.frame(name=property.name, value=property.value, stringsAsFactors = FALSE))
#    return(event)
#}
#
#
##' Create Network Provenance Entity
##'
##' @param uri URI of the resource described by the ProvenanceEntity. This field will not be set in some cases, such as a file upload or an algorithmic event that generates a network without a prior network as input.
##' @param creationEvent ProvenanceEvent - Has semantics of PROV:wasGeneratedBy
##' @param property.name array of names for name-value pairs
##' @param property.value array of values for name-value pairs
##' 
##' @return Network ProvenanceEntity Object
##' @examples 
##' \dontrun{}
##' NULL
#ndex.create.provenance.entity <- function(uri=NA, creationEvent, property.name, property.value){
#    entity = data.frame(uri=uri, creationEvent=NA, properties=NA)
#    entity$creationEvent = as.list(entity$creationEvent)
#    entity$creationEvent = list(creationEvent)
#    entity$properties = as.list(entity$properties)
#    entity$properties = list(data.frame(name=property.name, value=property.value, stringsAsFactors = FALSE))
#    return(entity)
#}
#
##' Create a default Network ProvenanceEntity object for upload
##'
##' @return Network ProvenanceEntity Object
##' @examples 
##' \dontrun{}
##' NULL
#ndex.create.provenance.entity.upload <- function(){
##ProvenanceEntity : data.frame
##    uri : String
##    creationEvent (ProvenanceEvent) : data.frame
##        inputs : data.frame
##        startedAtTime : String
##        endedAtTime : String
##        eventType : String
##        properties : list[[1]] : data.frame
##    properties : list[[1]] : data.frame
#    
#    event = ndex.create.provenance.event(    startedAtTime = '2017-03-23T09:00:00', 
#                                            endedAtTime = '2017-03-23T09:01:00', 
#                                            eventType = 'File Upload', 
#                                            property.name = c(    'user',
#                                                                'account name',    
#                                                                'filename'), 
#                                            property.value = c(    'Barry Manilow',
#                                                                'barrymanilow',    
#                                                                'Mandy Pathway.cx'))
#    entity = ndex.create.provenance.entity(    uri='http://public.ndexbio.org/network/8c515a1c-633f-11e6-b0a6-06603eb7f303', 
#                                            creationEvent = event,
#                                            property.name = c(    'edge count',    
#                                                                'node count',
#                                                                'dc:title',
#                                                                'description'), 
#                                            property.value = c(    3,
#                                                                4,
#                                                                "Barry's Small Network",
#                                                                ''))
#    return(entity)
#}
