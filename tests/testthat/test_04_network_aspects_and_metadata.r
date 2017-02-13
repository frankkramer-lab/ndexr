library(ndexr)
context('Aspects and meta-data')


test_that('Get network meta-data (ndex.network.get.metadata)', {
  nms = names(ndex.api.config)
  apiVersions = nms[nms!='defaultVersion']
  netColNames = c("consistencyGroup", "elementCount", "lastUpdate", "name", "properties", "version", "idCounter")
  
  con = ndex.connect()
  
  networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## public ndex account networks
  uuid = networks[1,'externalId']
  
  for(apiVersion in apiVersions){
    api = ndex.api.config[[apiVersion]]
    con = ndex.connect(apiConfig = api)
    rcx = ndex.network.get.metadata(con, uuid)
	if(con$apiConfig$version == '1.3'){
		expect_null(rcx, info=paste0('Aspect meta-data should work, but for some reason for api 1.3 only returns NULL!'))
	}else{
	    expect_is(rcx, 'list', info=paste0('Checking class of aspect meta-data (api ', apiVersion, ')'))
		expect_identical(names(rcx), 'metaData', info=paste0('Checking if returned object contains meta-data (api ', apiVersion, ')'))
		expect_object_conains_names(rcx$metaData, netColNames, info=paste0('Checking meta-data column names (api ', apiVersion, ')'))		
	}
  }
})

test_that('Get network aspect meta-data (ndex.network.aspect.get.metaData)', {
	nms = names(ndex.api.config)
	apiVersions = nms[nms!='defaultVersion']
	metColNames = c("consistencyGroup", "elementCount", "data", "lastUpdate", "name", "properties", "version", "idCounter")
	aspects = c("provenanceHistory", "nodes", "edges", "supports", "citations", "edgeAttributes", "edgeCitations", "edgeSupports", "networkAttributes", "nodeAttributes")
	
	con = ndex.connect()
	
	networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## public ndex account networks
	uuid = networks[1,'externalId']
	metData  = ndex.network.get.metadata(con, uuid)
	metDataNames = metData$metaData$name
	
	for(apiVersion in apiVersions){
		api = ndex.api.config[[apiVersion]]
		con = ndex.connect(apiConfig = api)
		for(asp in metDataNames){
			if(con$apiConfig$version == '1.3'){
				expect_error(ndex.network.aspect.get.metadata(con, uuid, asp), info=paste0('In api version 1.3 is no method for getting meta-data for single aspects ("', asp,'")'))
			}else{
				rcx = ndex.network.aspect.get.metadata(con, uuid, asp)
				expect_is(rcx, 'list', info=paste0('Checking class of aspect meta-data (api ', apiVersion, ', aspect ', asp, ')'))
				expect_object_conains_names(rcx, metColNames, info=paste0('Checking class of aspect meta-data (api ', apiVersion, ', aspect ', asp, ')'))
			}			
		}
	}
})

#test_that('Get network summary from server', {
#  nms = names(ndex.api.config)
#  apiVersions = nms[nms!='defaultVersion']
#  netColNames = c("ownerUUID", "isReadOnly", "visibility", "edgeCount", "nodeCount", "uri", "version", "owner", "description", "name", "externalId","modificationTime", "creationTime")
#
#  con = ndex.connect()
#  expect_error(ndex.network.get.summary(), info='No connection provided')
#  expect_error(ndex.network.get.summary(con), info='No networkUUID provided')
#  
#  previousRCX = NULL
#  previousVersion = NULL
#  
#  networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## public ndex account networks
#  uuid = networks[1,'externalId']
#  
#  for(apiVersion in apiVersions){
#    api = ndex.api.config[[apiVersion]]
#    con = ndex.connect(apiConfig = api)
#    netSum = ndex.network.get.summary(con, uuid)
#    expect_is(netSum, 'list', info=paste0('Checking class of found network (api ', apiVersion, ')'))
#    expect_object_conains_names(netSum, netColNames, info=paste0('Checking column names of found network (api ', apiVersion, ')'))
#    expect_equal(netSum[['externalId']], uuid, info=paste0('The found network should have the same uuid, that was used for quering it (api ', apiVersion, ')'))
#    
#    if(! is.null(previousRCX)) expect_identical(netSum, previousRCX, info=paste0('All api versions should retrieve the same rcx (api ', apiVersion, ' vs. ', previousVersion, ')'))
#    previousRCX = netSum
#    previousVersion = apiVersion
#  }
#})
#
#
#test_that('Create, update and delete a network on the server', {
#  nms = names(ndex.api.config)
#  apiVersions = nms[nms!='defaultVersion']
#  netColNames = c("ownerUUID", "isReadOnly", "visibility", "edgeCount", "nodeCount", "uri", "version", "owner", "description", "name", "externalId","modificationTime", "creationTime")
#  
#  con = ndex.connect(ndex$user, ndex$password)
#  expect_error(ndex.create.network(), info='No connection provided for creating a network')
#  expect_error(ndex.create.network(con), info='No CX object provided for creating a network')
#  expect_error(ndex.update.network(), info='No connection provided for updating a network')
#  expect_error(ndex.update.network(con), info='No CX object provided for updating a network')
#  expect_error(ndex.delete.network(), info='No connection provided for deleting a network')
#  expect_error(ndex.delete.network(con), info='No UUID provided for deleting a network')
#  
#  networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## public ndex account networks
#  uuid = networks[1,'externalId']
#  rcx = ndex.get.network(con, uuid)
#  
#  for(apiVersion in apiVersions){
#    api = ndex.api.config[[apiVersion]]
#    con = ndex.connect(ndex$user, ndex$password, apiConfig = api)
#    rcx$networkAttributes[1,'v']=paste0('Testing CRUD (create) with testthat (api ', apiVersion, ')')
#    
#    uuidCreated = ndex.create.network(con,rcx)
#    expect_that(uuidCreated, matches('?[0-9abcdef]{8}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{12}$'), info=paste0('Validate the returned uuid of created network (api ', apiVersion, ')'))
#    rcx$networkAttributes[1,'v']=paste0('Testing CRUD (update by manually set UUID) with testthat (api ', apiVersion, ')')
#    uuidUpdated =  ndex.update.network(con, rcx, uuidCreated)
#    expect_that(uuidUpdated, matches('?[0-9abcdef]{8}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{12}$'), info=paste0('Validate the returned uuid of updated network (api ', apiVersion, ')'))
#    rcx$networkAttributes[1,'v']=paste0('Testing CRUD (update by rcx UUID) with testthat (api ', apiVersion, ')')
#    rcx$ndexStatus$externalId = uuidCreated
#    uuidUpdatedRcx =  ndex.update.network(con,rcx)
#    expect_that(uuidUpdatedRcx, matches('?[0-9abcdef]{8}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{4}-[0-9abcdef]{12}$'), info=paste0('Validate the returned uuid of updated network by rcx (api ', apiVersion, ')'))
#    expect_equal(uuidCreated, uuidUpdated, info=paste0('Create and Update should have the same uuid (api ', apiVersion, ')'))
#    expect_equal(uuidUpdated, uuidUpdatedRcx, info=paste0('Update by rcx and manually set should have the same uuid (api ', apiVersion, ')'))
#    expect_null(ndex.delete.network(con, uuidUpdatedRcx), info=paste0('Returns NULL if network is successfully deleted (api ', apiVersion, ')'))
#    expect_error(ndex.delete.network(con, uuidUpdatedRcx), info=paste0('Deleting the same network again should throw an error (api ', apiVersion, ')'))
#    expect_error(ndex.delete.network(con, 'not-a-network-at-all'), info=paste0('Deleting the not existing network should throw an error (api ', apiVersion, ')'))
#  }
# })