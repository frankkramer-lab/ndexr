################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 05 February 2017 by Auer
##     
## Description:
##    Tests for RCX data model:
##    Check from and to JSON (rcx_fromJSON, rcx_toJSON)
##    Check new RCX objects (rcx_new)
##    Check MetaData update (rcx_updateMetaData)
##
## Usage:
##  devtools::test(filter='05_*')
################################################################################

library(ndexr)
context('RCX data model')


test_that('Check from and to JSON (rcx_fromJSON, rcx_toJSON)', {
    netColNames = c("metaData", "numberVerification", "ndexStatus", "@context", "citations", "edgeCitations", "edges", "networkAttributes", "nodeAttributes", "nodes", "status")
    
    con = ndex_connect()
    uuid = ndexTestConf$uuidPublicNetwork
    rcx = ndex_get_network(con, uuid)
    
    ## only test for core aspects
    rcx[!(names(rcx) %in% netColNames)]=NULL
    
    rcx2json = rcx_toJSON(rcx)
    rcx2json2rcx = rcx_fromJSON(rcx2json)
    
    expect_is(rcx, 'list', info=paste0('Checking class of rcx'))
    expect_is(rcx, 'RCX', info=paste0('Checking class of rcx'))
    expect_equal(rcx, rcx2json2rcx, info=paste0('A conversion from rcx to json and back to rcx must produce the same rcx object'))
})

test_that('Check new RCX objects (rcx_new)', {
    rcx1 = rcx_new()                                        # default
    rcx2 = rcx_new(c('@id'=1))                                # with node
    rcx3 = rcx_new(nodes=c('@id'=1))                        # name with node
    rcx4 = rcx_new(data.frame('@id'=c(1), check.names=F))    # data.frame
    
    expect_is(rcx1, 'list', info=paste0('Checking class of rcx (list)'))
    expect_is(rcx1, 'RCX', info=paste0('Checking class of rcx (RCX)'))
    expect_is(rcx2, 'list', info=paste0('Checking class of rcx (with node) (list)'))
    expect_is(rcx2, 'RCX', info=paste0('Checking class of rcx (with node) (RCX)'))
    expect_is(rcx3, 'list', info=paste0('Checking class of rcx (name with node) (list)'))
    expect_is(rcx3, 'RCX', info=paste0('Checking class of rcx (name with node) (RCX)'))
    expect_is(rcx4, 'list', info=paste0('Checking class of rcx (data.frame) (list)'))
    expect_is(rcx4, 'RCX', info=paste0('Checking class of rcx (data.frame) (RCX)'))
    expect_equal(rcx1, rcx2, info=paste0('The two functions (default and with node) should return the same rcx object'))
    expect_equal(rcx1, rcx3, info=paste0('The two functions (default and name with node)should return the same rcx object'))
    expect_equal(rcx1, rcx4, info=paste0('The two functions (default and data.frame)should return the same rcx object'))

    rcxV = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))                                    # vector
    rcxD = rcx_new(data.frame('@id'=c(1),n=c('Some Name'), r=c('HGNC:Symbol'), check.names=F))    # data.frame
    
    expect_is(rcxV, 'list', info=paste0('Checking class of rcx (vector) (list)'))
    expect_is(rcxV, 'RCX', info=paste0('Checking class of rcx (vector) (RCX)'))
    expect_is(rcxD, 'list', info=paste0('Checking class of rcx (data.frame) (list)'))
    expect_is(rcxD, 'RCX', info=paste0('Checking class of rcx (data.frame) (RCX)'))
    expect_equal(rcxV, rcxD, info=paste0('The two functions (from vector and data.frame) should return the same rcx object'))
})

test_that('Check MetaData update (rcx_updateMetaData)', {
    con = ndex_connect()
#    networks = ndex_find_networks(con, accountName = 'ndextutorials')  ## public ndex account networks
#    uuid = networks[1,'externalId']
    uuid = ndexTestConf$uuidPublicNetwork
    rcx = ndex_get_network(con, uuid)
    rcxF = rcx_updateMetaData(rcx,force=T)        # forced metaData update
    
    expect_is(rcxF, 'list', info=paste0('Checking class of rcx (list)'))
    expect_is(rcxF, 'RCX', info=paste0('Checking class of rcx (RCX)'))
    
    nodesIndex = which(rcxF$metaData$name=='nodes')
    rcxF$metaData[nodesIndex,'version']='6.6.6'
    rcxV6 = rcx_updateMetaData(rcxF)
    nodesV6Index = which(rcxV6$metaData$name=='nodes')
    expect_equal(rcxV6$metaData[nodesV6Index,'version'],'6.6.6', info=paste0('Changes in the metaData should be kept'))
    
    rcxS = rcx_updateMetaData(rcxF,force=T)
    rcxS$metaData = rcxS$metaData[-nodesIndex,]
    rcxS$citations = NULL
    rcxS = rcx_updateMetaData(rcxS)
    rcxT = rcx_updateMetaData(rcxF,force=T)
    citationsIndex = which(rcxT$metaData$name=='citations')
    rcxT$metaData = rcxT$metaData[-citationsIndex,]
    rownames(rcxT$metaData) = 1:dim(rcxT$metaData)[1]
    expect_equal(rcxS$metaData, rcxT$metaData, info=paste0('Missing aspects should be added/removed from the metaData'))
    
    rcxS$nodes = NULL
    expect_error(rcx_updateMetaData(rcxS),'*',info='A mandatory aspect is missing')
})


