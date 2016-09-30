###load
require(devtools)
install_github("frankkramer-lab/ndexr@develop")
library(ndexr)

###connect anonymously
ndexcon1 = ndex.connect(verbose=T)
###connect as test user
ndexcon2 = ndex.connect(username="testacc", password="testacc", verbose=T)

###get network api
apidata1 = ndex.get.network.api(ndexcon1)
apidata2 = ndex.get.network.api(ndexcon2)

###find some networks containing p53
pws1 = ndexr::ndex.find.networks(ndexcon1,"p53")
pws2 = ndexr::ndex.find.networks(ndexcon2,"p53")
###dont find networks
is.null(ndexr::ndex.find.networks(ndexcon1,"sdjlbelglserglersg"))
is.null(ndexr::ndex.find.networks(ndexcon2,"sdjlbelglserglersg"))

###get network details
pwsummary1 = ndex.get.network.summary(ndexcon1,pws1[1,"externalId"])
pwsummary2 = ndex.get.network.summary(ndexcon1,pws1[1,"externalId"])
###dont get network details
is.null(ndex.get.network.summary(ndexcon1,"sdjlbelglserglersg"))
is.null(ndex.get.network.summary(ndexcon2,"sdjlbelglserglersg"))

###get complete network as RCX
rcx1 = ndex.get.complete.network(ndexcon1,pws1[1,"externalId"])
rcx2 = ndex.get.complete.network(ndexcon2,pws1[1,"externalId"])

###convert to ngraph and back
ngraph1 = ndex.RCX2ngraph(rcx1)
rcx_back1 = ndex.ngraph2RCX(ngraph1)

##test equality of conversion:
for(i in names(rcx1)) {
  cat(i)
  cat(all.equal(rcx1[[i]], rcx_back1[[i]]))
  cat("\n")
}

### lets run through all the p53 pathways NDEx - RCX - ngraph - RCX
rcxlist=list()
rcxconvlist=list()
ngraphlist = list()
for(i in pws1$externalId) {
  rcxlist[[i]] = ndex.get.complete.network(ndexcon1,i)
  ngraphlist[[i]] = ndex.RCX2ngraph(rcxlist[[i]])
  rcxconvlist[[i]] = ndex.ngraph2RCX(ngraphlist[[i]])
}
### plot the graphs - no beautification yet
pdf(paper = "a4",file="../ndexr_testplot.pdf")
for(i in names(ngraphlist)) {
  g = ngraphlist[[i]]
  plot(g, vertex.label=V(g)$n, edge.label=E(g)$i)
}
dev.off()


## to come: save on NDEx server


