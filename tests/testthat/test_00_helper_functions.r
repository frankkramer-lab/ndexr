library(ndexr)
context('Helper funtions')



test_that('encode parameters in urls', {
  url = "http://en.wikipedia.org/w/index.php"
  values = c("Train", "5", "90", "history")
  result = "http://en.wikipedia.org/w/index.php/Train/5/90/history"
  expect_identical(ndex.helper.encodeParams(url, values=values), result, info='Encode values as path')
   
  params = c("title", "limit", "offset", "action") 
  namedParams = c(title="Train", limit="5", offset="90", action="history")
  result = "http://en.wikipedia.org/w/index.php?title=Train&limit=5&offset=90&action=history"
  expect_identical(ndex.helper.encodeParams(url, params=params, values=values), result, info='Encode params and values as params')
  expect_identical(ndex.helper.encodeParams(url, params, values), result, info='Encode params and values as params')          ## same as above, but shorter
  expect_identical(ndex.helper.encodeParams(url, params=namedParams), result, info='Encode params and values as params')      ## same as above, but with named params
  expect_identical(ndex.helper.encodeParams(url, namedParams), result, info='Encode params and values as params')             ## same as above, but shorter
   
  url = "http://en.wikipedia.org/w/index.php/#Train#/somethingElse/#Number#"
  namedParams = c(train="#Train#", someNumber="#Number#")
  values = c("ICE200", 12345)
  result = "http://en.wikipedia.org/w/index.php/ICE200/somethingElse/12345"
  expect_identical(ndex.helper.encodeParams(url, params=namedParams, values=values), result, info='Replace params with values in url')
  expect_identical(ndex.helper.encodeParams(url, namedParams, values), result, info='Replace params with values in url')
  
  expect_identical(ndex.helper.encodeParams(url), url, info='No params or values provided, so return the url')
})

test_that('Get the right api object', {
  con = ndex.connect()
  expect_identical(ndex.helper.getApi(con,'serverStatus'), con$apiConfig$api$serverStatus, info='The server status should work, else establishing an connection should have thrown an error!')
  expect_identical(ndex.helper.getApi(con,'network$summary$get'), con$apiConfig$api$network$summary$get, info='Testing some more complex query..')
  expect_error(ndex.helper.getApi(con,'network$aspect$create'), con$apiConfig$api$network$aspect$create, info='There should be no create function for aspects!!')
})