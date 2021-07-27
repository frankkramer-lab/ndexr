################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 05 February 2017 by Auer
##     
## Description:
##    Tests Aspects and meta-data:
##    Get network meta-data (ndex_network_get_metadata)
##    Get network aspect as CX (ndex_network_get_aspect)
##
## Usage:
##  devtools::test(filter='04_*')
################################################################################

library(ndexr)
context('Aspects and meta-data')


test_that('Get network meta-data (ndex_network_get_metadata)', {
    nms = names(ndex_config)
    apiVersions = nms[nms!='defaultVersion']
    # netColNames = c("consistencyGroup", "elementCount", "lastUpdate", "name", "properties", "version", "idCounter")
    ## no "properties" column anymore in metadata [2018.01.10]
    # netColNames = c("consistencyGroup", "elementCount", "lastUpdate", "name", "version", "idCounter")
    ## no "lastUpdate" column anymore in metadata, but the "properties" column returned! [2018.04.13]
    ## no "consistencyGroup" column anymore in metadata, optional now [2021.07.27]
    netColNames = c("elementCount", 
                    "name", 
                    "version", 
                    "idCounter")

    uuid = ndexTestConf$uuidPublicNetwork
    
    for(apiVersion in apiVersions){
        api = ndex_config[[apiVersion]]
        con = ndex_connect(ndexConf = api)
        if(con$ndexConf$version == '1.3'){
            ## 2018-01-08: was removed from public ndex server for some reason! 
            ## GET: [ www.ndexbio.org/rest/network/c9243cce-2d32-11e8-b939-0ac135e8bacf/metadata ]
            ## Some internal server error occurred (500)
            ## expect_null(rcx, info=paste0('Aspect meta-data should work, but for some reason for api 1.3 only returns NULL!'))
        }else{
            rcx = ndex_network_get_metadata(con, uuid)
            expect_is(rcx, 'data.frame', info=paste0('Checking class of aspect meta-data (api ', apiVersion, ')'))
            expect_object_conains_names(rcx, netColNames, info=paste0('Checking meta-data column names (api ', apiVersion, ')'))        
        }
    }
})


## Error on server side!
#
#test_that('Get network aspect meta-data (ndex_network_aspect_get_metadata)', {
#    nms = names(ndex_config)
#    apiVersions = nms[nms!='defaultVersion']
#    metColNames = c("consistencyGroup", "elementCount", "data", "lastUpdate", "name", "properties", "version", "idCounter")
#    aspects = c("provenanceHistory", "nodes", "edges", "supports", "citations", "edgeAttributes", "edgeCitations", "edgeSupports", "networkAttributes", "nodeAttributes")
#    
#    con = ndex_connect()
#    
##    networks = ndex_find_networks(con, accountName = 'ndextutorials')  ## public ndex account networks
##    uuid = networks[1,'externalId']
#    uuid = ndexTestConf$uuidPublicNetwork
#
#    metData  = ndex_network_get_metadata(con, uuid)
#    metDataNames = metData$metaData$name
#    
#    for(apiVersion in apiVersions){
#        api = ndex_config[[apiVersion]]
#        con = ndex_connect(ndexConf = api)
#        for(asp in metDataNames){
#            if(con$ndexConf$version == '1.3'){
#                expect_error(ndex_network_aspect_get_metadata(con, uuid, asp), info=paste0('In api version 1.3 is no method for getting meta-data for single aspects ("', asp,'")'))
#            }else{
#                rcx = ndex_network_aspect_get_metadata(con, uuid, asp)
#                expect_is(rcx, 'list', info=paste0('Checking class of aspect meta-data (api ', apiVersion, ', aspect ', asp, ')'))
#                expect_object_conains_names(rcx, metColNames, info=paste0('Checking class of aspect meta-data (api ', apiVersion, ', aspect ', asp, ')'))
#            }            
#        }
#    }
#})


test_that('Get network aspect as CX (ndex_network_get_aspect)', {
    nms = names(ndex_config)
    apiVersions = nms[nms!='defaultVersion']
    con = ndex_connect()
    
#    networks = ndex_find_networks(con, accountName = 'ndextutorials')  ## public ndex account networks
#    uuid = networks[1,'externalId']
    uuid = ndexTestConf$uuidPublicNetwork
    metData  = ndex_network_get_metadata(con, uuid)
    # metDataNames = metData$metaData$name[metData$metaData$elementCount>0]
    ## meta data is now returned directly as data.frame, and not nested anymore [2018.04.12] 
    metDataNames = metData$name[metData$elementCount>0]
    
    expect_is(metData, 'data.frame', info=paste0('Checking class of aspect (api ', apiVersion, ', aspect ', asp, ')'))
    
    for(apiVersion in apiVersions){
        api = ndex_config[[apiVersion]]
        con = ndex_connect(ndexConf = api)
        if(con$ndexConf$version != '1.3'){
            for(asp in metDataNames){
                rcx = ndex_network_get_aspect(con, uuid, asp)
                expect_is(rcx, 'data.frame', info=paste0('Checking class of aspect (api ', apiVersion, ', aspect ', asp, ')'))
            }
        }
    }
})

## Error on server side!
#
#test_that('Update network aspect (ndex_network_update_aspect)', {
#    nms = names(ndex_config)
#    apiVersions = nms[nms!='defaultVersion']
#    con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
#    
##    networks = ndex_find_networks(con, accountName = 'ndextutorials')  ## public ndex account networks
##    uuid = networks[1,'externalId']
#    uuid = ndexTestConf$uuidPrivateNetwork
#    rcx = ndex_get_network(con, uuid)
#    aspectNames = names(rcx)
#    aspectNames = aspectNames[aspectNames != 'metaData']
#    aspectNames = aspectNames[aspectNames != 'metaData']
#    
#    for(apiVersion in apiVersions){
#        api = ndex_config[[apiVersion]]
#        con = ndex_connect(ndexTestConf$user, ndexTestConf$password, ndexConf = api)
#        uuidCreated = ndex_create_network(con,rcx)
#        
#        if(con$ndexConf$version != '1.3'){
#            prevUuid = uuidCreated
#            for(asp in aspectNames){
#                aspData= rcx[[asp]]
#                aspDataMod = aspData[1:min(dim(aspData)[1],5),]
#                uuidUpdated = ndex_network_update_aspect(con, uuidCreated, asp, aspDataMod)
#                expect_equal(uuidUpdated, prevUuid, info=paste0('All uuids (created and updated) should have the same uuid (api ', apiVersion, ')'))
#                prevUuid = uuidUpdated
#            }        
#            rcxMod = ndex_get_network(con, uuidUpdated)
#            for(asp in aspectNames){
#                originalAspect = rcx[[asp]]
#                originalAspectMod = originalAspect[1:min(dim(originalAspect)[1],5),]
#                modAspect = rcxMod[[asp]]
#                
#                expect_is(modAspect, 'data.frame', info=paste0('Modificated aspects should be data.frames (api ', apiVersion, ')'))
#                expect_equal(dim(originalAspectMod), dim(modAspect), info=paste0('Modificated original and modificated aspects should have the same dimensions (api ', apiVersion, ')'))
#                expect_equal(sort(names(originalAspectMod)), sort(names(modAspect)), info=paste0('Modificated original and modificated aspects should have the same names (api ', apiVersion, ')'))
#            }
#            Sys.sleep(60)    ## Wait some time until the updating of the network on the server is done
#            ndex_delete_network(con, uuidUpdated)
#        }
#    }
#})

