library(ndexr)
context('Helper funtions')



test_that('encode parameters in urls', {
  ## replace
  url = "http://en.wikipedia.org/#NETWORKID#/index.php"
  params = list(	network=list(	tag="#NETWORKID#", method="replace"))
  values = c(network='aaaa-bb-cc-dddddd', bla='This is not used!')
  result = "http://en.wikipedia.org/aaaa-bb-cc-dddddd/index.php"
  expect_identical(ndex.helper.encodeParams(url, params=params, values), result, info='Encode params and values (replace)')
  
  values = c(bla='This is not used!')
  expect_error(ndex.helper.encodeParams(url, params=params, values), info='Encode params and (no) values (replace) -> error')
  
  params = list(	network=list(	tag="#NETWORKID#", method="replace", default="xxxx-xx-xx-xxxxxx"))
  result = "http://en.wikipedia.org/xxxx-xx-xx-xxxxxx/index.php"
  expect_identical(ndex.helper.encodeParams(url, params=params, values), result, info='Encode params and default values (replace)')
  
  ## parameter
  url = "http://en.wikipedia.org/w/index.php"
  params = list(	network=list(	tag="network", method="parameter"))
  values = c(network='aaaa-bb-cc-dddddd', bla='This is not used!')
  result = "http://en.wikipedia.org/w/index.php?network=aaaa-bb-cc-dddddd"
  expect_identical(ndex.helper.encodeParams(url, params=params, values), result, info='Encode params and values (parameter)')
  
  values = c(bla='This is not used!')
  expect_error(ndex.helper.encodeParams(url, params=params, values), info='Encode params and (no) values (parameter) -> error')
  
  params = list(	network=list(	tag="network", method="parameter", optional=TRUE))
  expect_identical(ndex.helper.encodeParams(url, params=params, values), url, info='Encode (optional) params and (no) values (parameter)')
  
  params = list(	network=list(	tag="network", method="parameter", default="xxxx-xx-xx-xxxxxx"))
  result = "http://en.wikipedia.org/w/index.php?network=xxxx-xx-xx-xxxxxx"
  expect_identical(ndex.helper.encodeParams(url, params=params, values), result, info='Encode params and (default) values (parameter)')
  
  values = c(network='aaaa-bb-cc-dddddd', bla='This is not used!')
  result = "http://en.wikipedia.org/w/index.php?network=aaaa-bb-cc-dddddd"
  expect_identical(ndex.helper.encodeParams(url, params=params, values), result, info='Encode params and (default) values (parameter, do not use default value)')
  
  ## append
  url = "http://en.wikipedia.org/w/index.php"
  params = list(	network=list(	method="append"))
  values = c(network='aaaa-bb-cc-dddddd', bla='This is not used!')
  result = "http://en.wikipedia.org/w/index.php/aaaa-bb-cc-dddddd"
  expect_identical(ndex.helper.encodeParams(url, params=params, values), result, info='Encode params and values (append)')
  
  values = c(bla='This is not used!')
  expect_error(ndex.helper.encodeParams(url, params=params, values), info='Encode params and (no) values (append) -> error')
  
  params = list(	network=list(	method="append", optional=TRUE))
  expect_identical(ndex.helper.encodeParams(url, params=params, values), url, info='Encode (optional) params and (no) values (append)')
  
  params = list(	network=list(	method="append", default="xxxx-xx-xx-xxxxxx"))
  result = "http://en.wikipedia.org/w/index.php/xxxx-xx-xx-xxxxxx"
  expect_identical(ndex.helper.encodeParams(url, params=params, values), result, info='Encode params and (default) values (append)')
  
  values = c(network='aaaa-bb-cc-dddddd', bla='This is not used!')
  result = "http://en.wikipedia.org/w/index.php/aaaa-bb-cc-dddddd"
  expect_identical(ndex.helper.encodeParams(url, params=params, values), result, info='Encode params and (default) values (append, do not use default value)')
  
})

test_that('Get the right api object', {
  con = ndex.connect()
  expect_identical(ndex.helper.getApi(con,'serverStatus'), con$apiConfig$api$serverStatus, info='The server status should work, else establishing an connection should have thrown an error!')
  expect_identical(ndex.helper.getApi(con,'network$summary$get'), con$apiConfig$api$network$summary$get, info='Testing some more complex query..')
  expect_error(ndex.helper.getApi(con,'network$aspect$create'), con$apiConfig$api$network$aspect$create, info='There should be no create function for aspects!!')
})