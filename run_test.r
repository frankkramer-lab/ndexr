##Tests
Sys.setenv('http_proxy' = '')
require(ndexr)

##Connect
ndex.connect('donshikin', '656NF41Z', host = 'test.ndexbio.org/rest')
ndex.alive()

##Find networks
ndex.find.networks("calmodulin")
x <- ndex.get.network.summary('32733ca9-4e84-11e4-98fb-000c29873918')
testnet <- ndex.get.complete.network('32733ca9-4e84-11e4-98fb-000c29873918')
testpg <- ndex.get.complete.network.as.property.graph('32733ca9-4e84-11e4-98fb-000c29873918')
testndex <- ndex.property.graph.as.ndexgraph(testpg)

testpg2 <- ndex.get.complete.network.as.property.graph('b5c2061b-5af0-11e4-bac2-000c29873918')
testndex2 <- ndex.property.graph.as.ndexgraph(testpg2)

#Convert to simpler format ()
# df_p53 <- ndexgraph2cbdd(p53, useNamespace='HGNC')
# 
# head(df_p53)
