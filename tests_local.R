###load
library(ndexr)

###connect anonymously
ndexcon1 = ndex.connect(verbose=T)
###connect as test user
ndexcon2 = ndex.connect(username="testacc", password="testacc", verbose=T)

###find some networks
pws1 = ndexr::ndex.find.networks(ndexcon1,"p53")
pws2 = ndexr::ndex.find.networks(ndexcon2,"p53")
###dont find networks
is.null(ndexr::ndex.find.networks(ndexcon1,"sdjlbelglserglersg"))
is.null(ndexr::ndex.find.networks(ndexcon2,"sdjlbelglserglersg"))

###get network details
pwsummary1 = ndex.get.network.summary(ndexcon1,pws[1,"externalId"])
pwsummary2 = ndex.get.network.summary(ndexcon1,pws[1,"externalId"])
###dont get network details
is.null(ndex.get.network.summary(ndexcon1,"sdjlbelglserglersg"))
is.null(ndex.get.network.summary(ndexcon2,"sdjlbelglserglersg"))

###get complete network
pw1 = ndex.get.complete.network(ndexcon1,pws[1,"externalId"])
pw2 = ndex.get.complete.network(ndexcon2,pws[1,"externalId"])


###get network api

apidata1 = get.network.api(ndexcon1)
apidata2 = get.network.api(ndexcon2)



#### get CX data
#cx1 = ndexr:::ndex_rest_GET(ndexcon1, route=paste0("/network/",pws[1,"externalId"],"/asCX"))
#content <- RCurl::getURL(paste0(ndexcon,"/network/",pws[1,"externalId"],"/asCX"),  .opts=ndexcon$curl.opts)
#parsedcontent = jsonlite::fromJSON(content, flatten=T)