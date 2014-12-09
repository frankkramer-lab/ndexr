##Tests
require(ndexr)
Sys.setenv('http_proxy' = '')

##Connect
ndex.connect('drh', 'drh', "http://test.ndexbio.org/rest", verbose = TRUE)
##ndex.alive()

##Find networks
nets <- ndex.find.networks("")
#print(nets)

# Get a network summary for the Hox_NVC4 network
summary <- ndex.get.network.summary("b5c2061b-5af0-11e4-bac2-000c29873918")
#print(summary)

# Search the large corpus for the neighborhood around MAPK1 and MAP2K1 
# should include connections between the two - MAPK1 is a target of the kinase MAP2K1 
npg <- ndexr:::ndex.get.neighborhood.as.property.graph("1ada3330-45cc-11e4-a9e5-000c29873918", searchString="MAPK1 MAP2K1")
##This now functions OK
neighborhood.ndexgraph <- ndex.property.graph.as.ndexgraph(npg)
str(neighborhood.ndexgraph)

testback <- ndexgraph.as.ndex.property.graph(neighborhood.ndexgraph)
##Not clear how to set name for a new network
##This is what seems to work in 'testpg' below (this is successfully uploaded, with different id but the same name)
newproperties <- data.frame('dc:title' = 'this_is_from_r')
testback$properties <- ndexr:::df2properties(newproperties)[[1]]
##This doesn't seem to affect save query in an example below
testback$name <- 'this_is_from_r'
ndexr:::ndex.save.new.property.graph.network(testback)


##Test saving back
testpg <- ndex.get.complete.network.as.property.graph('32733ca9-4e84-11e4-98fb-000c29873918')
##This name is not picked up when saving
testpg$name <- 'This_is_from_R'
testresponse <- ndexr:::ndex.save.new.property.graph.network(testpg)
ndex.find.networks("This_is_from_R", accountName = 'drh')
##There are multiple networks resulting from my attempts to resave Rudi's network under different name
##Strangely, they count only 11 nodes instead of original 28, although a graph has not been modified
##A bug?
ndex.find.networks("ca-calmodulin-rudi")

