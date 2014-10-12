##Tests
require(ndexr)

##Connect
ndex.connect('donshikin', '656NF41Z')
ndex.alive()

##Find networks
ndex.find.networks("p53", searchType='contains')
ndex.get.network.metadata('C25R636')

ndex.find.networks("LTR", searchType='contains')
ndex.get.network.metadata('C25R809', json=TRUE)

##Retrieve networks
p53 <- ndex.get.network('C25R286')
head(p53@nodes)
head(p53@edges)
head(p53@node_annot)
ltr2 <- ndex.get.network('C25R809')

#Convert to simpler format ()
df_p53 <- ndexgraph2cbdd(p53, useNamespace='HGNC')

head(df_p53)
