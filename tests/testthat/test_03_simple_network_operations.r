################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 05 February 2017 by Auer
##     
## Description:
##    Tests for Simple network operations:
##    Get a network from server (ndex_get_network)
##    Get network summary from server (ndex_network_get_summary)
##    Create, update and delete a network on the server (ndex_create_network, ndex_update_network, ndex_delete_network)
##
## Usage:
##  devtools::test(filter='03_*')
################################################################################

library(ndexr)
context('Simple network operations')

test_that('Get a network from server (ndex_get_network)', {
    nms = names(ndex_config)
      apiVersions = nms[nms!='defaultVersion']
      netColNames = c("metaData", 
                      "numberVerification", 
                      "ndexStatus", 
                      "@context", 
                      "citations", 
                      "edgeCitations", 
                      "edges", 
                      "networkAttributes", 
                      "nodeAttributes", 
                      "nodes", 
                      "status")

      con = ndex_connect()
      expect_error(ndex_get_network(), info='No connection provided')
      expect_error(ndex_get_network(con), info='No networkUUID provided')

      previousRCX = NULL
      previousVersion = NULL

#   networks = ndex_find_networks(con, accountName = 'ndextutorials')  ## public ndex account networks
#   uuid = networks[1,'externalId']
    uuid = ndexTestConf$uuidPublicNetwork    

  
      for(apiVersion in apiVersions){
        api = ndex_config[[apiVersion]]
        con = ndex_connect(ndexConf = api)
        rcx = ndex_get_network(con, uuid)
        expect_is(rcx, 'list', info=paste0('Checking class of found network (api ', apiVersion, ')'))
        expect_is(rcx, 'RCX', info=paste0('Checking class of found network (api ', apiVersion, ')'))
        expect_object_conains_names(rcx, netColNames, info=paste0('Checking column names of found network (api ', apiVersion, ')'))
        
        if(! is.null(previousRCX)) expect_identical(rcx, previousRCX, info=paste0('All api versions should retrieve the same rcx (api ', apiVersion, ' vs. ', previousVersion, ')'))
        previousRCX = rcx
        previousVersion = apiVersion
    }
})


test_that('Get network summary from server (ndex_network_get_summary)', {
    nms = names(ndex_config)
    apiVersions = nms[nms!='defaultVersion']
    # netColNames = c("ownerUUID", "isReadOnly", "visibility", "edgeCount", "nodeCount", "uri", "version", "owner", "description", "name", "externalId","modificationTime", "creationTime")
    ## no "uri" column anymore in summary [2018.04.13]
    netColNames = c("ownerUUID", 
                    "isReadOnly", 
                    "visibility", 
                    "edgeCount", 
                    "nodeCount",
                    "version", 
                    "owner", 
                    "description", 
                    "name", 
                    "externalId",
                    "modificationTime", 
                    "creationTime")
    
    con = ndex_connect()
    expect_error(ndex_network_get_summary(), info='No connection provided')
    expect_error(ndex_network_get_summary(con), info='No networkUUID provided')
      
    previousRCX = NULL
    previousVersion = NULL
      
#    networks = ndex_find_networks(con, accountName = 'ndextutorials')  ## public ndex account networks
#    uuid = networks[1,'externalId']
            
    uuid = ndexTestConf$uuidPublicNetwork
      
    for(apiVersion in apiVersions){
        api = ndex_config[[apiVersion]]
        con = ndex_connect(ndexConf = api)
        netSum = ndex_network_get_summary(con, uuid)
        expect_is(netSum, 'list', info=paste0('Checking class of found network (api ', apiVersion, ')'))
        expect_object_conains_names(netSum, netColNames, info=paste0('Checking column names of found network (api ', apiVersion, ')'))
        expect_equal(netSum[['externalId']], uuid, info=paste0('The found network should have the same uuid, that was used for quering it (api ', apiVersion, ')'))
        
        if(! is.null(previousRCX)) expect_identical(netSum, previousRCX, info=paste0('All api versions should retrieve the same rcx (api ', apiVersion, ' vs. ', previousVersion, ')'))
        previousRCX = netSum
        previousVersion = apiVersion
    }
})


# test_that('Create, update and delete a network on the server (ndex_create_network, ndex_update_network, ndex_delete_network)', {
#     nms = names(ndex_config)
#     apiVersions = nms[nms!='defaultVersion']
#     netColNames = c("ownerUUID", "isReadOnly", "visibility", "edgeCount", "nodeCount", "uri", "version", "owner", "description", "name", "externalId","modificationTime", "creationTime")
#   
#     con = ndex_connect()
#     expect_error(ndex_create_network(), info='No connection provided for creating a network')
#     expect_error(ndex_create_network(con), info='No CX object provided for creating a network')
#     expect_error(ndex_update_network(), info='No connection provided for updating a network')
#     expect_error(ndex_update_network(con), info='No CX object provided for updating a network')
#     expect_error(ndex_delete_network(), info='No connection provided for deleting a network')
#     expect_error(ndex_delete_network(con), info='No UUID provided for deleting a network')
#   
# #  networks = ndex_find_networks(con, accountName = 'ndextutorials')  ## public ndex account networks
# #  uuid = networks[1,'externalId']
#     uuid = ndexTestConf$uuidPublicNetwork
#       rcx = ndex_get_network(con, uuid)
#     networks = list()
#   
#     for(apiVersion in apiVersions){
#         api = ndex_config[[apiVersion]]
#         con = ndex_connect(ndexTestConf$user, ndexTestConf$password, ndexConf = api)
#         rcx$networkAttributes[1,'v']=paste0('Testing CRUD (create) with testthat (api ', apiVersion, ')')
# 
#         uuidCreated = ndex_create_network(con,rcx)
#         networks[[apiVersion]] = uuidCreated
#         expect_that(uuidCreated, matches('?[0-9abcdef]{8}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{12}$'), info=paste0('Validate the returned uuid of created network (api ', apiVersion, ')'))
#         
#         rcx$networkAttributes[1,'v']=paste0('Testing CRUD (update by manually set UUID) with testthat (api ', apiVersion, ')')
#         uuidUpdated =  ndex_update_network(con, rcx, uuidCreated)
#         expect_that(uuidUpdated, matches('?[0-9abcdef]{8}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{12}$'), info=paste0('Validate the returned uuid of updated network (api ', apiVersion, ')'))
#         
#         rcx$networkAttributes[1,'v']=paste0('Testing CRUD (update by rcx UUID) with testthat (api ', apiVersion, ')')
#         rcx$ndexStatus$externalId = uuidCreated
#         uuidUpdatedRcx =  ndex_update_network(con,rcx)
#         expect_that(uuidUpdatedRcx, matches('?[0-9abcdef]{8}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{12}$'), info=paste0('Validate the returned uuid of updated network by rcx (api ', apiVersion, ')'))
#         expect_equal(uuidCreated, uuidUpdated, info=paste0('Create and Update should have the same uuid (api ', apiVersion, ')'))
#         expect_equal(uuidUpdated, uuidUpdatedRcx, info=paste0('Update by rcx and manually set should have the same uuid (api ', apiVersion, ')'))
#     }
#     
#     sec = 10
#     cat(paste0('(wait ',sec,'sec)'))
#     Sys.sleep(sec)    ## Wait some time until the updating of the network on the server is done
#     
#     for(apiVersion in apiVersions){
#         api = ndex_config[[apiVersion]]
#         con = ndex_connect(ndexTestConf$user, ndexTestConf$password, ndexConf = api)
#         
#         expect_null(ndex_delete_network(con, networks[[apiVersion]]), info=paste0('Returns NULL if network is successfully deleted (api ', apiVersion, ')'))
#         expect_error(ndex_delete_network(con, networks[[apiVersion]]), info=paste0('Deleting the same network again should throw an error (api ', apiVersion, ')'))
#         expect_error(ndex_delete_network(con, 'not-a-network-at-all'), info=paste0('Deleting the not existing network should throw an error (api ', apiVersion, ')'))
#     }
# })