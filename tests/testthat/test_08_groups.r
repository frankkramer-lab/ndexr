################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 24 March 2017 by Auer
##     
## Description:
##    Tests for NDEx group settings:
##    CRUD functions of groups (ndex.create.group, ndex.update.group, ndex.get.group, ndex.delete.group)
##    Update user information (ndex.group.list.users)
##    Check group network permissions (ndex.group.list.networks, ndex.group.get.network)
##
## Usage:
##  devtools::test(filter='08_*')
################################################################################

library(ndexr)
context('Group settings')


test_that('CRUD functions of groups (ndex.create.group, ndex.update.group, ndex.get.group, ndex.delete.group)', {      
    con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
    userByName = ndex.find.user.byName(con, ndexTestConf$user)
    groupId = ndex.create.group(con, paste0('SomeTestGroup', paste0(sample(0:9,10,replace = T), collapse = '')))    # url (including the UUID) of the newly created group
    groupId = unlist(strsplit(groupId, '/', fixed = T))
    groupId = groupId[length(groupId)]    # get the UUID
    expect_equal(ndex.update.group(con, groupId, description='A really nice group!'),'', info='The result on a succesful update is a empty string ("")')
    
    group = ndex.get.group(con, groupId)
    expect_identical(group$description, 'A really nice group!', info=paste0('Description of the group should be updated.'))
    expect_identical(group$externalId, groupId, info=paste0('Ids of the group should be the same for created and updated.'))
    expect_null(ndex.delete.group(con, groupId), info='The result on a succesful delete is NULL')
})


test_that('Update user information (ndex.group.list.users)', {      
    con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
    users = ndex.group.list.users(con, ndexTestConf$uuidGroup)
    expect_true(ndexTestConf$user %in% users$memberAccountName, info='The user must be member of the group')
})

test_that('Check group network permissions (ndex.group.list.networks, ndex.group.get.network)', {      
    con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
    networks = ndex.group.list.networks(con, ndexTestConf$uuidGroup)
    expect_true(ndexTestConf$uuidPrivateNetwork %in% names(networks), info=paste0('The list of groups has to contain the defined groupId'))
    
    network = ndex.group.get.network(con, ndexTestConf$uuidGroup, ndexTestConf$uuidPrivateNetwork)
    expect_identical(networks[ndexTestConf$uuidPrivateNetwork], network, info=paste0('The result for a single network permission should be the same as for the same network permission in the list of network permissions!'))
})

