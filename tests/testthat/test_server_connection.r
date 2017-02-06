library(ndexr)
context('Connection to NDEx server')



test_that('establish connection to a server', {
  expect_object_conains_names = function(obj, names){
	  expect_true(all(names %in% names(obj)), 
			  paste0('object does not contain all names: ["',paste(names[!(names %in% names(obj))], collapse = '", "'), '"]'))
  }
  
  ndex.conf$defaults$apiversion = '1.3'
  
  con = ndex.connect()
  print(con)
  expect_is(con, 'NDExConnection')
  expect_object_conains_names(con, c("anonymous", "host", "apiversion", "verbose"))
  
  
  con = ndex.connect('testacc', 'testacc')
  expect_is(con, 'NDExConnection')
  expect_object_conains_names(con, c("anonymous", "host", "apiversion", "verbose", "username", "password"))
  
  expect_error(ndex.connect(host='google.com'))
})