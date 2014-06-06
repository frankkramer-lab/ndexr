##Tests
Sys.setenv('http_proxy'='')
require(ndexr)
ndex.connect('donshikin', '656NF41Z')
ndex.alive()
ndex.find.networks("p53", searchType='contains')
ndex.get.network.metadata('C25R636')

ndex.find.networks("LTR", searchType='contains')
ndex.get.network.metadata('C25R809', json=TRUE)

p53 <- ndex.get.network('C25R286')
ltr2 <- ndex.get.network('C25R809')



cbdd_p53 <- ndexgraph2cbdd(p53, useNamespace='HGNC')
cbdd_ltr2 <- ndexgraph2cbdd(ltr2, useNamespace='HGNC')

View(ct)
