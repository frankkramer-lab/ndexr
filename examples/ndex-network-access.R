##Tests
require(ndexr)

##Connect
ndexcon = ndex.connect('drh', 'drh', "http://test.ndexbio.org/rest")

summary <- ndex.get.network.summary(ndexcon, "b5c2061b-5af0-11e4-bac2-000c29873918", FALSE)
#print(summary)

propertygraph <- ndex.get.complete.network.as.property.graph(ndexcon, "b5c2061b-5af0-11e4-bac2-000c29873918")

ndexgraph <- ndex.property.graph.as.ndexgraph(propertygraph)

print(ndexgraph)

