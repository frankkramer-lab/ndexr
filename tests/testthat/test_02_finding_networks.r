################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 05 February 2017 by Auer
##     
## Description:
##    Tests for Finding networks:
##    Get list of networks from a server (ndex.find.networks)
##
## Usage:
##  devtools::test(filter='02_*')
################################################################################

library(ndexr)
context('Finding networks')



test_that('Get list of networks from a server (ndex.find.networks)', {
  nms = names(ndex.conf)
  apiVersions = nms[nms!='defaultVersion']
  netColNames = c("ownerUUID", "isReadOnly", "subnetworkIds", "errorMessage", "isValid", "warnings", "isShowcase", "visibility", "edgeCount", "nodeCount", "uri", "version", "owner", "description", "name", "properties", "externalId", "isDeleted", "modificationTime", "creationTime")
  
  expect_error(ndex.find.networks(), info='No connection provided')
  
  for(apiVersion in apiVersions){
    api = ndex.conf[[apiVersion]]
    con = ndex.connect(ndexConf = api)
    
    networks = ndex.find.networks(con)
    expect_is(networks, 'data.frame', info=paste0('Checking class of found network list using api ', apiVersion))
    expect_object_conains_names(networks, netColNames, info=paste0('Checking column names of found network list using api ', apiVersion))
    forLater = networks[6:10,]
    
    networks = ndex.find.networks(con, searchString = 'egfr')
    expect_is(networks, 'data.frame', info=paste0('Checking class of found network list ("egfr") using api ', apiVersion))
    expect_object_conains_names(networks, netColNames, info=paste0('Checking column names of found network list ("egfr") using api ', apiVersion))
    
    expect_null(ndex.find.networks(con, accountName = 'aNonExistingAccountThatHasThereforeNoPublicNetworks'), info='No networks should be found and therefore the return value be NULL')
    
    networks = ndex.find.networks(con, accountName = 'ndextutorials')
    expect_is(networks, 'data.frame', info=paste0('Checking class of found network list ("egfr") of "ndextutorials" using api ', apiVersion))
    expect_object_conains_names(networks, netColNames, info=paste0('Checking column names of found network list ("egfr") of "ndextutorials" using api ', apiVersion))
    
    networks = ndex.find.networks(con, start=1, size=5)
    rownames(networks) = NULL
    rownames(forLater) = NULL
    columnsToBeEqual = c("ownerUUID", "isReadOnly", "visibility", "edgeCount", "nodeCount", "uri", "owner", "name", "externalId","modificationTime", "creationTime")

    expect_is(networks, 'data.frame', info=paste0('Checking class of found network list (start=1, size=5) using api ', apiVersion))
    expect_object_conains_names(networks, netColNames, info=paste0('Checking column names of found network list (start=1, size=5) using api ', apiVersion))
    expect_equal(dim(networks)[1], 5, info=paste0('The found network list (start=1, size=5) should have a 5 rows using api ', apiVersion))
    expect_identical(networks[,columnsToBeEqual], 
                     forLater[,columnsToBeEqual], 
                     info=paste0('The network list (start=1, size=5) should be the same as the excerpt [6:10] with standard parameter (start=0, size=100) using api ', apiVersion))
  }
})