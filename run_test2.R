##Tests
require(ndexr)

##Connect
ndex.connect('drh', 'drh', "http://test.ndexbio.org/rest")
##ndex.alive()

##Find networks
nets <- ndex.find.networks("gal", "drh")

cat(nets)

