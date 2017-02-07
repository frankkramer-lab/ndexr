library(ndexr)
context('Connection to NDEx server')



test_that('establish connection to a server', {
  expect_object_conains_names = function(object, expected, info = NULL) {
    comp = all(expected %in% names(object))
    mesg = ifelse(comp, paste0('objectect does not contain all names: ["', paste(object[!(expected %in% names(object))], collapse = '", "'), '"]\n',
                  'expected names: ["', paste(expected, collapse = '", "'), '"]\n',
                  'found names: ["', paste(names(object), collapse = '", "'), '"]\n'),
                  '')
    expect( comp,
            mesg,
            info = info)
    invisible(object)
  }
  
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
  con = ndex.connect('testacc', 'testacc')
  expect_is(con, 'NDExConnection', info='Default connection with credentials')
  expect_object_conains_names(con, conNamesCredentials, info='Default connection with credentials')
  ## Test default connection with credentials using invalid parameter
  expect_error(ndex.connect('NotAnExistingAccount', 'password'), info='Default connection with credentials')
  expect_error(ndex.connect('testacc', 'testacc', host='google.com'), info='Default connection with credentials')
  expect_error(ndex.connect('testacc', 'testacc', apiPath = '/deadEnd'), info='Default connection with credentials')
  expect_error(ndex.connect('testacc', 'testacc', apiConfig = NULL), info='Default connection with credentials')
  
  for(apiVersion in apiVersions){
    api = ndex.api.config[[apiVersion]]
    ## Test connection with api
    con = ndex.connect(apiConfig = api)
    expect_is(con, 'NDExConnection', info=paste0('Connection using api ', apiVersion))
    expect_object_conains_names(con, conNamesDefault, info=paste0('Connection using api ', apiVersion))
    expect_equal(con$apiConfig$version, api$version, info=paste0('Connection using api ', apiVersion))
    ## Test connection with api and credentials
    con = ndex.connect('testacc', 'testacc', apiConfig = api)
    expect_is(con, 'NDExConnection', info=paste0('Connection using api ', apiVersion, ' and credentials'))
    expect_object_conains_names(con, conNamesCredentials, info=paste0('Connection using api ', apiVersion, ' and credentials'))
    expect_equal(con$apiConfig$version, api$version, info=paste0('Connection using api ', apiVersion, ' and credentials'))
  }

})