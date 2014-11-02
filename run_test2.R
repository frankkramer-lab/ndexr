##Tests
require(ndexr)

##Connect
ndex.connect('drh', 'drh', "http://test.ndexbio.org/rest")
##ndex.alive()

##Find networks
# nets <- ndex.find.networks("")

net <- ndex.get.network.summary("b5c2061b-5af0-11e4-bac2-000c29873918", TRUE)
cat(net)


