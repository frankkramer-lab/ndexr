################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 05 February 2017 by Auer
##     
## Description:
##    Tests for NDEx properties:
##    Set network network system properties (ndex_network_set_systemProperties)
##    Update network profile (ndex_network_set_systemProperties
##    Get network provenance (ndex_network_get_provenance)
##
## Usage:
##  devtools::test(filter='06_*')
################################################################################

library(ndexr)
context('NDEx properties')


# test_that('Set network network system properties (ndex_network_set_systemProperties)', {
#     nms = names(ndex_config)
#     apiVersions = nms[nms!='defaultVersion']
#       
#     con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
# #    networks = ndex_find_networks(con, accountName = 'ndextutorials')  ## get networks from public ndex account
# #    uuid = networks[1,'externalId']
#     uuid = ndexTestConf$uuidPrivateNetwork
#     
#     for(apiVersion in apiVersions){
#         api = ndex_config[[apiVersion]]
#         con = ndex_connect(ndexTestConf$user, ndexTestConf$password, ndexConf = api)
#     
#         expect_error(ndex_network_set_systemProperties(con, uuid), info=paste0('At least one property has to be specified! (api ', apiVersion, ')'))
#         
#         result = ndex_network_set_systemProperties(con, uuid, readOnly=TRUE)
#         expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
#     
#         if(con$ndexConf$version == '1.3'){
#             result = ndex_network_set_systemProperties(con, uuid, readOnly=FALSE)
#             expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
#         }else{
#             result = ndex_network_set_systemProperties(con, uuid, showcase=TRUE)
#             expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
#             
#             result = ndex_network_set_systemProperties(con, uuid, readOnly=FALSE, showcase=FALSE)
#             expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
#         }
#       }
# })

# test_that('Update network profile (ndex_network_set_systemProperties)', {
#     nms = names(ndex_config)
#     apiVersions = nms[nms!='defaultVersion']
#     
#     con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
# #    networks = ndex_find_networks(con, accountName = 'ndextutorials')  ## get networks from public ndex account
# #    uuid = networks[1,'externalId']
#     uuid = ndexTestConf$uuidPrivateNetwork
# #    rcx = ndex_get_network(con, uuid)
# #    uuid = ndex_create_network(con,rcx)    ## and upload it as own one
#     summ = ndex_network_get_summary(con, uuid)
#     
#     for(apiVersion in apiVersions){
#         api = ndex_config[[apiVersion]]
#         con = ndex_connect(ndexTestConf$user, ndexTestConf$password, ndexConf = api)
#         
#         expect_error(ndex_network_update_profile(con, uuid), info=paste0('At least one property has to be specified! (api ', apiVersion, ')'))
#         
#         result = ndex_network_update_profile(con, uuid, name=paste0("Some fancy name for the network (",apiVersion,')'))
#         expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
#         
#         result = ndex_network_update_profile(con, uuid, description=paste0("Description of the network (",apiVersion,')'))
#         expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
#         
#         result = ndex_network_update_profile(con, uuid, version=paste0("1.2.3.4 (",apiVersion,')'))
#         expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
#         
#         result = ndex_network_update_profile(con, uuid, name=summ$name, description=summ$description, version=summ$version)
#         expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
#     }
# })


test_that('Get network provenance (ndex_network_get_provenance)', {
    nms = names(ndex_config)
    apiVersions = nms[nms!='defaultVersion']
    
    con = ndex_connect()
#    networks = ndex_find_networks(con, accountName = 'ndextutorials')  ## get networks from public ndex account
#    uuid = networks[1,'externalId']
    uuid = ndexTestConf$uuidPublicNetwork
#    rcx = ndex_get_network(con, uuid)
#    uuid = ndex_create_network(con,rcx)    ## and upload it as own one
    
    for(apiVersion in apiVersions){
        api = ndex_config[[apiVersion]]
        con = ndex_connect(ndexConf = api)
        
        provenance = ndex_network_get_provenance(con, uuid)
        expect_object_conains_names(provenance, c("uri","creationEvent","properties"), info=paste0('Checking attribute names of network provenance (api ', apiVersion, ')'))
        expect_object_conains_names(provenance$creationEvent, c("inputs","startedAtTime","endedAtTime","eventType","properties"), info=paste0('Checking attribute names of network provenance attribute "creationEvent" (api ', apiVersion, ')'))
    }
})

