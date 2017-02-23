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


test_that('Get network aspect as CX (ndex.network.get.aspect)', {
	nms = names(ndex.api.config)
	apiVersions = nms[nms!='defaultVersion']
	con = ndex.connect()
	
	networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## public ndex account networks
	uuid = networks[1,'externalId']
	metData  = ndex.network.get.metadata(con, uuid)
	metDataNames = metData$metaData$name[metData$metaData$elementCount>0]
	
	for(apiVersion in apiVersions){
		api = ndex.api.config[[apiVersion]]
		con = ndex.connect(apiConfig = api)
		if(con$apiConfig$version != '1.3'){
			for(asp in metDataNames){
				rcx = ndex.network.get.aspect(con, uuid, asp)
				expect_is(rcx, 'data.frame', info=paste0('Checking class of aspect (api ', apiVersion, ', aspect ', asp, ')'))
			}
		}
	}
})

## Error on server side!
#
#test_that('Update network aspect (ndex.network.update.aspect)', {
#	nms = names(ndex.api.config)
#	apiVersions = nms[nms!='defaultVersion']
#	con = ndex.connect()
#	
#	networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## public ndex account networks
#	uuid = networks[1,'externalId']
#	rcx = ndex.get.network(con, uuid)
#	aspectNames = names(rcx)
#	aspectNames = aspectNames[aspectNames != 'metaData']
#	aspectNames = aspectNames[aspectNames != 'metaData']
#	
#	for(apiVersion in apiVersions){
#		api = ndex.api.config[[apiVersion]]
#		con = ndex.connect(ndex$user, ndex$password, apiConfig = api)
#		uuidCreated = ndex.create.network(con,rcx)
#		
#		if(con$apiConfig$version != '1.3'){
#			prevUuid = uuidCreated
#			for(asp in aspectNames){
#				aspData= rcx[[asp]]
#				aspDataMod = aspData[1:min(dim(aspData)[1],5),]
#				uuidUpdated = ndex.network.update.aspect(con, uuidCreated, asp, aspDataMod)
#				expect_equal(uuidUpdated, prevUuid, info=paste0('All uuids (created and updated) should have the same uuid (api ', apiVersion, ')'))
#				prevUuid = uuidUpdated
#			}		
#			rcxMod = ndex.get.network(con, uuidUpdated)
#			for(asp in aspectNames){
#				originalAspect = rcx[[asp]]
#				originalAspectMod = originalAspect[1:min(dim(originalAspect)[1],5),]
#				modAspect = rcxMod[[asp]]
#				
#				expect_is(modAspect, 'data.frame', info=paste0('Modificated aspects should be data.frames (api ', apiVersion, ')'))
#				expect_equal(dim(originalAspectMod), dim(modAspect), info=paste0('Modificated original and modificated aspects should have the same dimensions (api ', apiVersion, ')'))
#				expect_equal(sort(names(originalAspectMod)), sort(names(modAspect)), info=paste0('Modificated original and modificated aspects should have the same names (api ', apiVersion, ')'))
#			}
#			Sys.sleep(60)	## Wait some time until the updating of the network on the server is done
#			ndex.delete.network(con, uuidUpdated)
#		}
#	}
#})

