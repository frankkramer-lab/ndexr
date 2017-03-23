################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 05 February 2017 by Auer
## 	
## Description:
##	Tests for NDEx properties:
##    Set network network system properties (ndex.network.set.systemProperties)
##    Update network profile (ndex.network.set.systemProperties
##    Get network provenance (ndex.network.get.provenance)
##
## Usage:
##  devtools::test(filter='06_*')
################################################################################

library(ndexr)
context('NDEx properties')


test_that('Set network network system properties (ndex.network.set.systemProperties)', {
	nms = names(ndex.api.config)
	apiVersions = nms[nms!='defaultVersion']
	  
	con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
#	networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## get networks from public ndex account
#	uuid = networks[1,'externalId']
	uuid = ndexTestConf$uuidPrivateNetwork
	
	for(apiVersion in apiVersions){
		api = ndex.api.config[[apiVersion]]
	    con = ndex.connect(ndexTestConf$user, ndexTestConf$password, apiConfig = api)
	
		expect_error(ndex.network.set.systemProperties(con, uuid), info=paste0('At least one property has to be specified! (api ', apiVersion, ')'))
		
		result = ndex.network.set.systemProperties(con, uuid, readOnly=TRUE)
	    expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
	
		if(con$apiConfig$version == '1.3'){
			result = ndex.network.set.systemProperties(con, uuid, readOnly=FALSE)
		    expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
		}else{
			result = ndex.network.set.systemProperties(con, uuid, showcase=TRUE)
		    expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
			
			result = ndex.network.set.systemProperties(con, uuid, readOnly=FALSE, showcase=FALSE)
		    expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
		}
  	}
})

test_that('Update network profile (ndex.network.set.systemProperties)', {
	nms = names(ndex.api.config)
	apiVersions = nms[nms!='defaultVersion']
	
	con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
#	networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## get networks from public ndex account
#	uuid = networks[1,'externalId']
	uuid = ndexTestConf$uuidPrivateNetwork
#	rcx = ndex.get.network(con, uuid)
#	uuid = ndex.create.network(con,rcx)	## and upload it as own one
	summ = ndex.network.get.summary(con, uuid)
	
	for(apiVersion in apiVersions){
		api = ndex.api.config[[apiVersion]]
		con = ndex.connect(ndexTestConf$user, ndexTestConf$password, apiConfig = api)
		
		expect_error(ndex.network.update.profile(con, uuid), info=paste0('At least one property has to be specified! (api ', apiVersion, ')'))
		
		result = ndex.network.update.profile(con, uuid, name=paste0("Some fancy name for the network (",apiVersion,')'))
		expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
		
		result = ndex.network.update.profile(con, uuid, description=paste0("Description of the network (",apiVersion,')'))
		expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
		
		result = ndex.network.update.profile(con, uuid, version=paste0("1.2.3.4 (",apiVersion,')'))
		expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
		
		result = ndex.network.update.profile(con, uuid, name=summ$name, description=summ$description, version=summ$version)
		expect_null(result, info=paste0('If everything works "NULL" should be returned (api ', apiVersion, ')'))
	}
})


test_that('Get network provenance (ndex.network.get.provenance)', {
	nms = names(ndex.api.config)
	apiVersions = nms[nms!='defaultVersion']
	
	con = ndex.connect()
#	networks = ndex.find.networks(con, accountName = 'ndextutorials')  ## get networks from public ndex account
#	uuid = networks[1,'externalId']
	uuid = ndexTestConf$uuidPublicNetwork
#	rcx = ndex.get.network(con, uuid)
#	uuid = ndex.create.network(con,rcx)	## and upload it as own one
	
	for(apiVersion in apiVersions){
		api = ndex.api.config[[apiVersion]]
		con = ndex.connect(apiConfig = api)
		
		provenance = ndex.network.get.provenance(con, uuid)
		expect_object_conains_names(provenance, c("uri","creationEvent","properties"), info=paste0('Checking attribute names of network provenance (api ', apiVersion, ')'))
		expect_object_conains_names(provenance$creationEvent, c("inputs","startedAtTime","endedAtTime","eventType","properties"), info=paste0('Checking attribute names of network provenance attribute "creationEvent" (api ', apiVersion, ')'))
	}
})

