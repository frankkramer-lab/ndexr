##Authors:
#   Alex Ishkin [aleksandr.ishkin@thomsonreuters.com]
#   Dexter Pratt [depratt@ucsd.edu]
#   Frank Kramer [frank.kramer@med.uni-goettingen.de]
#   Florian Auer [florian.auer@med.uni-goettingen.de]
##Created: 28 November 2016
# Contains API configuration for connecting to NDEx servers

##########################################################
# API conf



#' NDEx server api configuration
#' 
#' This nested list contains the url and methods for accessing the NDEx server via its REST full api
#' It contains specifications for NDEx server api version 1.3 and 2.0
#' If possible, the version 2.0 should be used
#' 
#' @return Nested list resembling the REST structure
#' @export
ndex.api = list(replaceables = list(uuid='#UUID#',
                                    network='#NETWORKID#',
                                    aspect='#ASPECT#',
                                    start='#START#',
                                    size='#SIZE#',
                                    permission='#PERMISSION#',
                                    key='#KEY#',
                                    value='#VALUE#',
                                    type='#TYPE#'),    ##type={user|group}
                serverStatus=list(description='Get Server Status',
                                  '2.0'=list(url='/admin/status',
                                             method='GET'),
                                  '1.3'=list(url='/admin/status',
                                             method='GET')),
                user=list(authenticate=list(description='Authenticate a User',
                                          '2.0'=list(url='/user?valid=true',
                                                     method='GET'),
                                          '1.3'=list(url='/user/authenticate',
                                                     method='GET'))),
                network=list(create=list( description='Create a CX Network',
                                          '2.0'=list(url='/network',
                                                     method='POST'),
                                          '1.3'=list(url='/network/asCX',
                                                     method='POST')),
                             update=list( description='Update a Network',
                                          '2.0'=list(url='/network/#NETWORKID#',
                                                     method='PUT'),
                                          '1.3'=list(url='/network/asCX/#NETWORKID#',
                                                     method='PUT')),
                             delete=list( description='Delete a Network',
                                          '2.0'=list(url='/network/#NETWORKID#',
                                                     method='POST'),
                                          '1.3'=list(url='/network/#NETWORKID#',
                                                     method='DELETE')),
                             get=list( description='Get Complete Network in CX format',
                                       '2.0'=list(url='/network/#NETWORKID#',
                                                  method='GET'),
                                       '1.3'=list(url='/network/#NETWORKID#/asCX',
                                                  method='GET')),
                             summary=list(  get=list( description='Get Network Summary',
                                                      '2.0'=list(url='/network/#NETWORKID#/summary',
                                                                 method='GET'),
                                                      '1.3'=list(url='/network/#NETWORKID#/summary',
                                                                 method='GET'))),
                             sample=list(  set=list( description='Get Network Summary',
                                                     '2.0'=list(url='/network/#NETWORKID#/sample',
                                                                method='PUT')),
                                           get=list( description='Set Network Sample',
                                                      '2.0'=list(url='/network/#NETWORKID#/sample',
                                                                 method='GET'))),
                             aspect=list(  getMetaData=list( description='Get Network CX Metadata Collection',
                                                             '2.0'=list(url='/network/#NETWORKID#/aspect',
                                                                        method='GET'),
                                                             '1.3'=list(url='/network/#NETWORKID#/metadata',
                                                                        method='GET')),
                                           getMetaDataByName=list( description='Get Network Aspect Metadata',
                                                                   '2.0'=list(url='/network/#NETWORKID#/aspect/#ASPECT#/metadata',
                                                                              method='GET')),
                                           get=list( description='Get a Network Aspect As CX',
                                                      '2.0'=list(url='/network/#NETWORKID#/aspect/#ASPECT#?',
                                                                 method='GET',
                                                                 params=list(size='#SIZE#')),
                                                      '1.3'=list(url='/network/#NETWORKID#/aspect/#ASPECT#/#SIZE#',
                                                                 method='GET')),
                                           update=list( description='Update an Aspect of a Network',
                                                         '2.0'=list(url='/network/#NETWORKID#/aspect/#ASPECT#',
                                                                    method='PUT'))),
                             permission=list(  get=list( description='Get All Permissions on a Network',
                                                         '2.0'=list(url='/network/#NETWORKID#/permission?',
                                                                    method='GET',
                                                                    params=list(type='#TYPE#', 
                                                                                permission='#PERMISSION#', 
                                                                                start='#START#', 
                                                                                size='#SIZE#')),
                                                         '1.3'=list(url='/network/#NETWORKID#/user/#PERMISSION#/#START#/#SIZE#',
                                                                    method='GET')),
                                               update=list( description='Update Network Permission',
                                                         '2.0'=list(url='/network/#NETWORKID#/permission?',
                                                                    method='PUT',
                                                                    params=list(userid='#UUID#', 
                                                                                groupid='#UUID#', 
                                                                                permission='#PERMISSION#')),
                                                         '1.3'=list(url='/network/#NETWORKID#/member',
                                                                    method='PUT')),
                                               delete=list( description='Delete Network Permission',
                                                            '2.0'=list(url='/network/#NETWORKID#/permission?',
                                                                       method='DELETE',
                                                                       params=list(userid='#UUID#', 
                                                                                   groupid='#UUID#'),
                                                             '1.3'=list(url='/network/#NETWORKID#/member/#UUID#',
                                                                        method='DELETE')))),
                             profile=list(  update=list( description='Update Network Profile',
                                                         '2.0'=list(url='/network/#NETWORKID#/profile',
                                                                    method='PUT'))),
                             systemproperties=list(  set=list( description='Set Network System Properties',
                                                               '2.0'=list(url='/network/#NETWORKID#/systemproperty',
                                                                          method='PUT'),
                                                               '1.3'=list(url='/network/#NETWORKID#/setFlag/#KEY#=#VALUE#',
                                                                          method='PUT'))),
                             properties=list(  set=list( description='Set Network Properties',
                                                         '2.0'=list(url='/network/#NETWORKID#/properties',
                                                                    method='PUT'),
                                                         '1.3'=list(url='/network/#NETWORKID#/properties',
                                                                    method='PUT'))),
                             provenance=list(  set=list( description='Set Network Provenance',
                                                         '2.0'=list(url='/network/#NETWORKID#/provenance',
                                                                    method='PUT'),
                                                         '1.3'=list(url='/network/#NETWORKID#/provenance',
                                                                    method='PUT')),
                                               get=list( description='Get Network Provenance',
                                                         '2.0'=list(url='/network/#NETWORKID#/provenance',
                                                                    method='GET'),
                                                         '1.3'=list(url='/network/#NETWORKID#/provenance',
                                                                    method='GET')))),
                search=list(user=list( description='Search users',
                                       '2.0'=list(url='/search/user?',
                                                  method='POST',
                                                  params=list(start='#START#',
                                                              size='#SIZE#')),
                                       '1.3'=list(url='/user/search/#START#/#SIZE#',
                                                  method='POST')),
                            group=list( description='Search groups',
                                        '2.0'=list(url='/search/group?',
                                                   method='POST',
                                                   params=list(start='#START#',
                                                               size='#SIZE#')),
                                        '1.3'=list(url='/group/search/#START#/#SIZE#',
                                                   method='POST')),
                            network=list( search=list( description='Search network',
                                                       '2.0'=list(url='/search/network?',
                                                                  method='POST',
                                                                  params=list(start='#START#',
                                                                              size='#SIZE#')),
                                                       '1.3'=list(url='/network/textsearch/#START#/#SIZE#',
                                                                  method='POST')),
                                          neighborhood=list( description='Query Network As CX',
                                                             '2.0'=list(url='/search/network/#NETWORKID#/query?',
                                                                        method='POST',
                                                                        params=list(size='#SIZE#')),
                                                             '1.3'=list(url='/network/#NETWORKID#/asCX/query',
                                                                        method='POST')))))



#' Search networks in NDEx (by description)
#' 
#' This functions searches the public networks on an NDEx server for networks containing the supplied search string. T
#' his search can be limited to certain accounts as well as in length.
#' 
#' @param ndexcon object of class NDEXConnection
#' @param searchString string by which to search
#' @param accountName string; constrain search to networks administered by this account
#' @param skipBlocks -
#' @param blockSize -
#' @return Data frame with network information: ID, name, whether it is public, edge and node count; source and format of network. NULL if no networks are found.
#' @section REST query:
#' This function runs POST query /network/search/#START#/#SIZE#    returns list of NetworkSummary
#' @note Search strings may be structured
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws1 = ndex.find.networks(ndexcon1,"p53") }
#' @export
ndex.api.addParams = function(url, replaceable, value){
  paste0(url,paste(replaceable,value, sep = '=', collapse = '&'))
}

