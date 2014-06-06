##Tests
Sys.setenv('http_proxy'='')
ndex.connect('donshikin', '656NF41Z')
ndex.alive()
ndex.find.networks("p53", searchType='contains')
m <- ndex.get.network.metadata('C25R286')
ndex.get.network.metadata('C25R636')

ndex.get.network.metadata('C25R286', json=TRUE)
t <- ndex.get.network('C25R286')


ct <- ndexgraph2cbdd(t, useNamespace='HGNC')
View(ct)
