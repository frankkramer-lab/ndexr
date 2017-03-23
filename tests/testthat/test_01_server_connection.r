################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 05 February 2017 by Auer
## 	
## Description:
##	Test the connection to NDEx server:
##    Establish connection to a server (ndex.connect)
##
## Usage:
##  devtools::test(filter='01_*')
################################################################################

library(ndexr)
context('Connection to NDEx server')


test_that('Establish connection to a server (ndex.connect)', {
  nms = names(ndex.api.config)
  apiVersions = nms[nms!='defaultVersion']
  
  conNamesDefault = c("anonymous", "host", "apiConfig", "verbose")
  conNamesCredentials = c(conNamesDefault, "username", "password")
  
  ## Test default connection
  con = ndex.connect()
  expect_is(con, 'NDExConnection', info='Default connection')
  expect_object_conains_names(con, conNamesDefault, info='Default connection')
  ## Test default connection using invalid parameter
  expect_error(ndex.connect(host='google.com'), info='Default connection')
  expect_error(ndex.connect(apiPath = '/deadEnd'), info='Default connection')
  expect_error(ndex.connect(apiConfig = NULL), info='Default connection')
  ## Test defaul connection with credentials
  con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
  expect_is(con, 'NDExConnection', info='Default connection with credentials')
  expect_object_conains_names(con, conNamesCredentials, info='Default connection with credentials')
  ## Test default connection with credentials using invalid parameter
  expect_error(ndex.connect('NotAnExistingAccount', 'password'), info='Default connection with credentials')
  expect_error(ndex.connect(ndexTestConf$user, ndexTestConf$password, host='google.com'), info='Default connection with credentials')
  expect_error(ndex.connect(ndexTestConf$user, ndexTestConf$password, apiPath = '/deadEnd'), info='Default connection with credentials')
  expect_error(ndex.connect(ndexTestConf$user, ndexTestConf$password, apiConfig = NULL), info='Default connection with credentials')
  
  for(apiVersion in apiVersions){
    api = ndex.api.config[[apiVersion]]
    ## Test connection with api
    con = ndex.connect(apiConfig = api)
    expect_is(con, 'NDExConnection', info=paste0('Connection using api ', apiVersion))
    expect_object_conains_names(con, conNamesDefault, info=paste0('Connection using api ', apiVersion))
    expect_equal(con$apiConfig$version, api$version, info=paste0('Connection using api ', apiVersion))
    ## Test connection with api and credentials
    con = ndex.connect(ndexTestConf$user, ndexTestConf$password, apiConfig = api)
    expect_is(con, 'NDExConnection', info=paste0('Connection using api ', apiVersion, ' and credentials'))
    expect_object_conains_names(con, conNamesCredentials, info=paste0('Connection using api ', apiVersion, ' and credentials'))
    expect_equal(con$apiConfig$version, api$version, info=paste0('Connection using api ', apiVersion, ' and credentials'))
  }

})