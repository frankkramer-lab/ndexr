################################################################################
## Authors:
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 05 February 2017 by Auer
##     
## Description:
##     Default configuration for the user (and password) used for testing
##   The "ndexTestConf" object is a simple list, which must have the following proberties defined:
##      user:                 User name
##      password:             Corresponding user password
##      uuidPrivateNetwork: UUID of a private network (owned by the defined user, shared with a group) to perform some (network changing) tests on
##      uuidPublicNetwork:     UUID of a public network (usually from "ndextutorials") to perform some read-only tests
##      uuidGroup:            UUID of a (private) group, to that the specified user belongs
##   Furthermore, the user must own a network, which is shared with the group!
##
## Usage:
##  devtools::test(filter='05_*')
################################################################################
ndexTestConf=list(    user='testacc', password='testacc',             # username and password to test the server connection
            #uuidPublicNetwork='9ed0cd55-9ac0-11e4-9499-000c29202374',    # doesn't work anymore April 2018
            uuidPublicNetwork='c9243cce-2d32-11e8-b939-0ac135e8bacf',    # UUID of a public network (usually from "ndextutorials") to perform some read-only tests
            uuidPrivateNetwork='9f1ca938-0e1f-11e7-ab16-0ac135e8bacf',    # UUID of a private network (owned by the defined user) to perform some (network changing) tests on
            uuidGroup='ac4cb95c-08c8-11e7-aba2-0ac135e8bacf')    # UUID of a (private) group, to that the specified user belongs
