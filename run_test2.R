##Tests
require(ndexr)

##Connect
ndex.connect('drh', 'drh', "http://test.ndexbio.org/rest")
##ndex.alive()

##Find networks
nets <- ndex.find.networks("")
#print(nets)

# Get a network summary for the Hox_NVC4 network
summary <- ndex.get.network.summary("b5c2061b-5af0-11e4-bac2-000c29873918")
#print(summary)

# Search the large corpus for the neighborhood around MAPK1 and MAP2K1 
# should include connections between the two - MAPK1 is a target of the kinase MAP2K1 
neighborhood.property.graph <- ndex.get.neighborhood.as.property.graph("1ada3330-45cc-11e4-a9e5-000c29873918", searchString="MAPK1 MAP2K1")

neighborhood.ndexgraph <- ndex.property.graph.as.ndexgraph(neighborhood.property.graph)

print(neighborhood.ndexgraph)


