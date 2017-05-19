################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 24 March 2017 by Auer
##     
## Description:
##    Tests for NDEx group settings:
##    CRUD functions of groups (ndex_create_group, ndex_update_group, ndex_get_group, ndex_delete_group)
##    Update user information (ndex_group_list_users)
##    Check group network permissions (ndex_group_list_networks, ndex_group_network_get_permission)
##
## Usage:
##  devtools::test(filter='08_*')
################################################################################

library(ndexr)
context('Group settings')


# test_that('CRUD functions of groups (ndex_create_group, ndex_update_group, ndex_get_group, ndex_delete_group)', {      
#     con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
#     userByName = ndex_find_user_byName(con, ndexTestConf$user)
#     groupId = ndex_create_group(con, paste0('SomeTestGroup', paste0(sample(0:9,10,replace = T), collapse = '')))    # url (including the UUID) of the newly created group
#     groupId = unlist(strsplit(groupId, '/', fixed = T))
#     groupId = groupId[length(groupId)]    # get the UUID
#     expect_equal(ndex_update_group(con, groupId, description='A really nice group!'),'', info='The result on a succesful update is a empty string ("")')
#     
#     group = ndex_get_group(con, groupId)
#     expect_identical(group$description, 'A really nice group!', info=paste0('Description of the group should be updated.'))
#     expect_identical(group$externalId, groupId, info=paste0('Ids of the group should be the same for created and updated.'))
#     expect_null(ndex_delete_group(con, groupId), info='The result on a succesful delete is NULL')
# })


# test_that('Update user information (ndex_group_list_users)', {      
#     con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
#     users = ndex_group_list_users(con, ndexTestConf$uuidGroup)
#     expect_true(ndexTestConf$user %in% users$memberAccountName, info='The user must be member of the group')
# })

# test_that('Check group network permissions (ndex_group_list_networks, ndex_group_network_get_permission)', {      
#     con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
#     networks = ndex_group_list_networks(con, ndexTestConf$uuidGroup)
#     expect_true(ndexTestConf$uuidPrivateNetwork %in% names(networks), info=paste0('The list of groups has to contain the defined groupId'))
#     
#     network = ndex_group_network_get_permission(con, ndexTestConf$uuidGroup, ndexTestConf$uuidPrivateNetwork)
#     expect_identical(networks[ndexTestConf$uuidPrivateNetwork], network, info=paste0('The result for a single network permission should be the same as for the same network permission in the list of network permissions!'))
# })

