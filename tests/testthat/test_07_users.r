################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 24 March 2017 by Auer
##     
## Description:
##    Tests for NDEx user settings:
##    Find user (ndex_find_user_byName, ndex_find_user_byId)
##    Update user information (ndex_update_user)
##    Change password (ndex_user_change_password)
##    Check group membership (ndex_user_list_groups, ndex_user_show_group)
##    Check group network permissions (ndex_user_list_permissions, ndex_user_show_permission)
##
## Usage:
##  devtools::test(filter='07_*')
################################################################################

library(ndexr)
context('User settings')


test_that('Find user (ndex_find_user_byName, ndex_find_user_byId)', {      
    # con = ndex_connect(ndexTestConf$user, ndexTestConf$password)    
    con = ndex_connect()
    userByName = ndex_find_user_byName(con, ndexTestConf$user)
    userById = ndex_find_user_byId(con, userByName$externalId)
    expect_identical(userByName, userById, info=paste0('The results for both functions should be the same.'))
})


# test_that('Update user information (ndex_update_user)', {      
#     con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
#     userByName = ndex_find_user_byName(con, ndexTestConf$user)
#     userChanged = ndex_update_user(con, userByName$externalId, isIndividual=FALSE, displayName='ChangedName', firstName='ChangedFirstName', lastName='ChangedLastName', image='http://www.ndexbio.org/img/ndex2.0-new-logo_850w.png', website='www.ndexbio.org/#/', description='ChangedDescription')
#     userBack = ndex_update_user(con, userByName$externalId, isIndividual=TRUE, displayName='TestAccount', firstName='Homer', lastName='Simpson', image='http://www.ndexbio.org/img/NDEx-horizontal-transparent.png', website='www.ndexbio.org', description='Call me Homer Jay')
#     
#     expect_identical(userBack, "", info=paste0('The return of an update function is an empty string ("")'))
# })

# test_that('Change password (ndex_user_change_password)', {      
#     con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
#     userByName = ndex_find_user_byName(con, ndexTestConf$user)
#     newPassword = 'SomeRandomNewPassword'
#     userPasswordChanged = ndex_user_change_password(con, userByName$externalId, newPassword)
#     expect_error(ndex_connect(ndexTestConf$user, ndexTestConf$password), info=paste0("After changing the password the login shouldn't be possigble anymore"))
#     
#     con = ndex_connect(ndexTestConf$user, newPassword)
#     userPasswordChanged = ndex_user_change_password(con, userByName$externalId, ndexTestConf$password)
#     
#     con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
# })

# test_that('Check group membership (ndex_user_list_groups, ndex_user_show_group)', {      
#     con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
#     userByName = ndex_find_user_byName(con, ndexTestConf$user)
#     groups = ndex_user_list_groups(con, userByName$externalId)
#     expect_true(ndexTestConf$uuidGroup %in% names(groups), info=paste0('The list of groups has to contain the defined groupId'))
#     
#     group = ndex_user_show_group(con, userByName$externalId, ndexTestConf$uuidGroup)
#     expect_identical(groups[ndexTestConf$uuidGroup], group, info=paste0('The result for a single group should be the same as for the same group in the list of groups!'))
# })

# test_that('Check group network permissions (ndex_user_list_permissions, ndex_user_show_permission)', {      
#     con = ndex_connect(ndexTestConf$user, ndexTestConf$password)
#     userByName = ndex_find_user_byName(con, ndexTestConf$user)
#     networks = ndex_user_list_permissions(con, userByName$externalId)
#     expect_true(ndexTestConf$uuidPrivateNetwork %in% names(networks), info=paste0('The list of networks has to contain the defined network'))
#     
#     network = ndex_user_show_permission(con, userByName$externalId, ndexTestConf$uuidPrivateNetwork)
#     expect_identical(networks[ndexTestConf$uuidPrivateNetwork], network, info=paste0('The result for a single network permission should be the same as for the same network permission in the list of network permissions!'))
# })

