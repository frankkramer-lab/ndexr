################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 24 March 2017 by Auer
##     
## Description:
##    Tests for NDEx user settings:
##    Find user (ndex.find.user.byName, ndex.find.user.byId)
##    Update user information (ndex.update.user)
##    Change password (ndex.user.change.password)
##    Check group membership (ndex.user.list.groups, ndex.user.show.group)
##    Check group network permissions (ndex.user.list.permissions, ndex.user.show.permission)
##
## Usage:
##  devtools::test(filter='07_*')
################################################################################

library(ndexr)
context('User settings')


test_that('Find user (ndex.find.user.byName, ndex.find.user.byId)', {      
    # con = ndex.connect(ndexTestConf$user, ndexTestConf$password)    
    con = ndex.connect()
    userByName = ndex.find.user.byName(con, ndexTestConf$user)
    userById = ndex.find.user.byId(con, userByName$externalId)
    expect_identical(userByName, userById, info=paste0('The results for both functions should be the same.'))
})


# test_that('Update user information (ndex.update.user)', {      
#     con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
#     userByName = ndex.find.user.byName(con, ndexTestConf$user)
#     userChanged = ndex.update.user(con, userByName$externalId, isIndividual=FALSE, displayName='ChangedName', firstName='ChangedFirstName', lastName='ChangedLastName', image='http://www.ndexbio.org/img/ndex2.0-new-logo_850w.png', website='www.ndexbio.org/#/', description='ChangedDescription')
#     userBack = ndex.update.user(con, userByName$externalId, isIndividual=TRUE, displayName='TestAccount', firstName='Homer', lastName='Simpson', image='http://www.ndexbio.org/img/NDEx-horizontal-transparent.png', website='www.ndexbio.org', description='Call me Homer Jay')
#     
#     expect_identical(userBack, "", info=paste0('The return of an update function is an empty string ("")'))
# })

# test_that('Change password (ndex.user.change.password)', {      
#     con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
#     userByName = ndex.find.user.byName(con, ndexTestConf$user)
#     newPassword = 'SomeRandomNewPassword'
#     userPasswordChanged = ndex.user.change.password(con, userByName$externalId, newPassword)
#     expect_error(ndex.connect(ndexTestConf$user, ndexTestConf$password), info=paste0("After changing the password the login shouldn't be possigble anymore"))
#     
#     con = ndex.connect(ndexTestConf$user, newPassword)
#     userPasswordChanged = ndex.user.change.password(con, userByName$externalId, ndexTestConf$password)
#     
#     con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
# })

# test_that('Check group membership (ndex.user.list.groups, ndex.user.show.group)', {      
#     con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
#     userByName = ndex.find.user.byName(con, ndexTestConf$user)
#     groups = ndex.user.list.groups(con, userByName$externalId)
#     expect_true(ndexTestConf$uuidGroup %in% names(groups), info=paste0('The list of groups has to contain the defined groupId'))
#     
#     group = ndex.user.show.group(con, userByName$externalId, ndexTestConf$uuidGroup)
#     expect_identical(groups[ndexTestConf$uuidGroup], group, info=paste0('The result for a single group should be the same as for the same group in the list of groups!'))
# })

# test_that('Check group network permissions (ndex.user.list.permissions, ndex.user.show.permission)', {      
#     con = ndex.connect(ndexTestConf$user, ndexTestConf$password)
#     userByName = ndex.find.user.byName(con, ndexTestConf$user)
#     networks = ndex.user.list.permissions(con, userByName$externalId)
#     expect_true(ndexTestConf$uuidPrivateNetwork %in% names(networks), info=paste0('The list of networks has to contain the defined network'))
#     
#     network = ndex.user.show.permission(con, userByName$externalId, ndexTestConf$uuidPrivateNetwork)
#     expect_identical(networks[ndexTestConf$uuidPrivateNetwork], network, info=paste0('The result for a single network permission should be the same as for the same network permission in the list of network permissions!'))
# })

