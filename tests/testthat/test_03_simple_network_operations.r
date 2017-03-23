################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 05 February 2017 by Auer
## 	
## Description:
##	Tests for Simple network operations:
##    Get a network from server (ndex.get.network)
##    Get network summary from server (ndex.network.get.summary)
##    Create, update and delete a network on the server (ndex.create.network, ndex.update.network, ndex.delete.network)
##
## Usage:
##  devtools::test(filter='03_*')
################################################################################

library(ndexr)
context('Simple network operations')

test_that('Get a network from server (ndex.get.network)', {
	nms = names(ndex.api.config)
  	apiVersions = nms[nms!='defaultVersion']
  	netColNames = c("metaData", "numberVerification", "ndexStatus", "@context", "citations", "edgeCitations", "edges", "networkAttributes", "nodeAttributes", "nodes", "status")

  	con = ndex.connect()
  	expect_error(ndex.get.network(), info='No connection provided')
  	expect_error(ndex.get.network(con), info='No networkUUID provided')

  	previousRCX = NULL
  	previousVersion = NULL

#   networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## public ndex account networks
#   uuid = networks[1,'externalId']
	uuid = ndexTestConf$uuidPublicNetwork	

  
  	for(apiVersion in apiVersions){
	    api = ndex.api.config[[apiVersion]]
	    con = ndex.connect(apiConfig = api)
	    rcx = ndex.get.network(con, uuid)
	    expect_is(rcx, 'list', info=paste0('Checking class of found network (api ', apiVersion, ')'))
	    expect_is(rcx, 'RCX', info=paste0('Checking class of found network (api ', apiVersion, ')'))
	    expect_object_conains_names(rcx, netColNames, info=paste0('Checking column names of found network (api ', apiVersion, ')'))
	    
	    if(! is.null(previousRCX)) expect_identical(rcx, previousRCX, info=paste0('All api versions should retrieve the same rcx (api ', apiVersion, ' vs. ', previousVersion, ')'))
	    previousRCX = rcx
	    previousVersion = apiVersion
	}
})


test_that('Get network summary from server (ndex.network.get.summary)', {
	nms = names(ndex.api.config)
	apiVersions = nms[nms!='defaultVersion']
	netColNames = c("ownerUUID", "isReadOnly", "visibility", "edgeCount", "nodeCount", "uri", "version", "owner", "description", "name", "externalId","modificationTime", "creationTime")
	
	con = ndex.connect()
	expect_error(ndex.network.get.summary(), info='No connection provided')
	expect_error(ndex.network.get.summary(con), info='No networkUUID provided')
	  
	previousRCX = NULL
	previousVersion = NULL
	  
#	networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## public ndex account networks
#	uuid = networks[1,'externalId']
			
	uuid = ndexTestConf$uuidPublicNetwork
	  
	for(apiVersion in apiVersions){
	    api = ndex.api.config[[apiVersion]]
	    con = ndex.connect(apiConfig = api)
	    netSum = ndex.network.get.summary(con, uuid)
	    expect_is(netSum, 'list', info=paste0('Checking class of found network (api ', apiVersion, ')'))
	    expect_object_conains_names(netSum, netColNames, info=paste0('Checking column names of found network (api ', apiVersion, ')'))
	    expect_equal(netSum[['externalId']], uuid, info=paste0('The found network should have the same uuid, that was used for quering it (api ', apiVersion, ')'))
	    
	    if(! is.null(previousRCX)) expect_identical(netSum, previousRCX, info=paste0('All api versions should retrieve the same rcx (api ', apiVersion, ' vs. ', previousVersion, ')'))
	    previousRCX = netSum
	    previousVersion = apiVersion
	}
})


test_that('Create, update and delete a network on the server (ndex.create.network, ndex.update.network, ndex.delete.network)', {
    nms = names(ndex.api.config)
    apiVersions = nms[nms!='defaultVersion']
    netColNames = c("ownerUUID", "isReadOnly", "visibility", "edgeCount", "nodeCount", "uri", "version", "owner", "description", "name", "externalId","modificationTime", "creationTime")
  
    con = ndex.connect()
    expect_error(ndex.create.network(), info='No connection provided for creating a network')
    expect_error(ndex.create.network(con), info='No CX object provided for creating a network')
    expect_error(ndex.update.network(), info='No connection provided for updating a network')
    expect_error(ndex.update.network(con), info='No CX object provided for updating a network')
    expect_error(ndex.delete.network(), info='No connection provided for deleting a network')
    expect_error(ndex.delete.network(con), info='No UUID provided for deleting a network')
  
#  networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## public ndex account networks
#  uuid = networks[1,'externalId']
	uuid = ndexTestConf$uuidPublicNetwork
  	rcx = ndex.get.network(con, uuid)
	networks = list()
  
	for(apiVersion in apiVersions){
	    api = ndex.api.config[[apiVersion]]
	    con = ndex.connect(ndexTestConf$user, ndexTestConf$password, apiConfig = api)
	    rcx$networkAttributes[1,'v']=paste0('Testing CRUD (create) with testthat (api ', apiVersion, ')')

		uuidCreated = ndex.create.network(con,rcx)
		networks[[apiVersion]] = uuidCreated
		expect_that(uuidCreated, matches('?[0-9abcdef]{8}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{12}$'), info=paste0('Validate the returned uuid of created network (api ', apiVersion, ')'))
		
		rcx$networkAttributes[1,'v']=paste0('Testing CRUD (update by manually set UUID) with testthat (api ', apiVersion, ')')
		uuidUpdated =  ndex.update.network(con, rcx, uuidCreated)
		expect_that(uuidUpdated, matches('?[0-9abcdef]{8}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{12}$'), info=paste0('Validate the returned uuid of updated network (api ', apiVersion, ')'))
		
		rcx$networkAttributes[1,'v']=paste0('Testing CRUD (update by rcx UUID) with testthat (api ', apiVersion, ')')
		rcx$ndexStatus$externalId = uuidCreated
		uuidUpdatedRcx =  ndex.update.network(con,rcx)
		expect_that(uuidUpdatedRcx, matches('?[0-9abcdef]{8}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{12}$'), info=paste0('Validate the returned uuid of updated network by rcx (api ', apiVersion, ')'))
		expect_equal(uuidCreated, uuidUpdated, info=paste0('Create and Update should have the same uuid (api ', apiVersion, ')'))
		expect_equal(uuidUpdated, uuidUpdatedRcx, info=paste0('Update by rcx and manually set should have the same uuid (api ', apiVersion, ')'))
	}
	
	sec = 10
	cat(paste0('(wait ',sec,'sec)'))
	Sys.sleep(sec)	## Wait some time until the updating of the network on the server is done
	
	for(apiVersion in apiVersions){
		api = ndex.api.config[[apiVersion]]
		con = ndex.connect(ndexTestConf$user, ndexTestConf$password, apiConfig = api)
		
		expect_null(ndex.delete.network(con, networks[[apiVersion]]), info=paste0('Returns NULL if network is successfully deleted (api ', apiVersion, ')'))
		expect_error(ndex.delete.network(con, networks[[apiVersion]]), info=paste0('Deleting the same network again should throw an error (api ', apiVersion, ')'))
		expect_error(ndex.delete.network(con, 'not-a-network-at-all'), info=paste0('Deleting the not existing network should throw an error (api ', apiVersion, ')'))
	}
})