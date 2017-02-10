################################################################################
## Authors:
##   Frank Kramer [frank.kramer@med.uni-goettingen.de]
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##
## History:
##   Created on 20 September 2016 by Kramer
## 	
## Description:
##	Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


####################################################
## Conversion from/to JSON
####################################################

#' Create RCX object from JSON data
#' 
#' This function creates an RCX object from a supplied JSON-encoded CX object. It is usually called from within \code{\link{ndex.get.network}}.
#' RCX objects store the CX data as a named list of data.frames containing metaData and all aspects of the network.
#' 
#' The structure of an RCX object, as shown via str(rcx) could be a list like this:\cr
#' \preformatted{
#' > str(rcx)
#' 
#' List of 12
#' $ metaData          :'data.frame':	11 obs. of  7 variables:
#'   ..$ name            : chr [1:11] "citations" "@context" "edgeAttributes" "edgeCitations" ...
#'   ..$ consistencyGroup: int [1:11] 1 1 1 1 1 1 1 1 1 1 ...
#'   ..$ elementCount    : int [1:11] 4 23 NA NA 11 1 NA NA NA 5 ...
#'   ..$ lastUpdate      : num [1:11] 1.44e+12 1.44e+12 1.44e+12 1.44e+12 1.44e+12 ...
#'   ..$ version         : chr [1:11] "1.0" "1.0" "1.0" "1.0" ...
#'   ..$ idCounter       : int [1:11] 60714397 NA NA NA 60714399 NA NA NA NA 60714395 ...
#'   ..$ properties      :List of 11
#' $ numberVerification:'data.frame':	1 obs. of  1 variable:
#'   ..$ longNumber: num 2.81e+14
#' $ ndexStatus        :'data.frame':	1 obs. of  10 variables:
#'   ..$ externalId      : chr "eac8a4b8-6194-11e5-8ac5-06603eb7f303"
#'   ..$ creationTime    : num 1.44e+12
#'   ..$ modificationTime: num 1.44e+12
#'   ..$ visibility      : chr "PUBLIC"
#'   ..$ published       : logi FALSE
#'   ..$ nodeCount       : int 5
#'   ..$ edgeCount       : int 11
#'   ..$ owner           : chr "nci-pid"
#'   ..$ ndexServerURI   : chr "http://public.ndexbio.org"
#'   ..$ readOnly        : logi FALSE
#' $ @context          :'data.frame':	1 obs. of  23 variables:
#'   ..$ GENPEPT                      : chr "http://www.ncbi.nlm.nih.gov/protein/"
#'   ..$ NCBI GENE                    : chr "http://identifiers.org/ncbigene/"
#'   ..$ ENSEMBL                      : chr "http://identifiers.org/ensembl/"
#'   [...]
#' $ networkAttributes :'data.frame':	4 obs. of  2 variables:
#'   ..$ n: chr [1:4] "name" "description" "version" "ndex:sourceFormat"
#'   ..$ v: chr [1:4] "PLK3 signaling events" "This network ..." [...]
#' $ citations         :'data.frame':	4 obs. of  7 variables:
#'  ..$ @id           : int [1:4] 60714380 60714383 60714386 60714397
#'  ..$ dc:identifier : chr [1:4] "pmid:17264206" "pmid:14968113" "pmid:12242661" "pmid:11551930"
#'  ..$ dc:type       : chr [1:4] "URI" "URI" "URI" "URI"
#'  ..$ attributes    :List of 4 [...]
#' $ nodes             :'data.frame':	5 obs. of  2 variables:
#'  ..$ @id: int [1:5] 60714376 60714377 60714381 60714384 60714395
#'  ..$ n  : chr [1:5] "CCNE1" "PLK3" "MPIP3" "CHK2" ...
#' $ nodeAttributes    :'data.frame':	10 obs. of  4 variables:
#'   ..$ po: int [1:10] 60714376 60714376 60714377 60714377 60714381 60714381 60714384 60714384 60714395 60714395
#'   ..$ n : chr [1:10] "alias" "relatedTo" "alias" "relatedTo" ...
#'   ..$ v :List of 10
#'     .. ..$ : chr [1:6] "UniProt Knowledgebase:Q92501" "UniProt Knowledgebase:Q9UD21"  ...
#'     .. ..$ : chr [1:98] "GENE ONTOLOGY:GO:0003713" "GENE ONTOLOGY:GO:0005515"  ...
#'     [...]
#'   ..$ d : chr [1:10] "list_of_string"  ...
#' $ edges             :'data.frame':	11 obs. of  4 variables:
#'   ..$ @id: int [1:11] 60714379 60714382  ...
#'   ..$ s  : int [1:11] 60714376 60714381  ...
#'   ..$ t  : int [1:11] 60714377 60714377  ...
#'   ..$ i  : chr [1:11] "neighbor-of" "neighbor-of"  ...
#' $ edgeCitations     :'data.frame':	11 obs. of  2 variables:
#'   ..$ po       :List of 11
#'   .. ..$ : int 60714379
#'   .. ..$ : int 60714382
#'   [...]
#' ..$ citations:List of 11
#' .. ..$ : int 60714380
#' .. ..$ : int 60714383
#'   [...]
#' $ status            :'data.frame':	1 obs. of  2 variables:
#'   ..$ error  : chr ""
#'   ..$ success: logi TRUE
#'- attr(*, "class")= chr [1:2] "RCX" "list"
#' }
#' 
#' The data.frames representing nodes and edges could look like this:\cr
#' \preformatted{
#' > rcx[["nodes"]]
#'    @id     n
#'  1 60714376 CCNE1
#'  2 60714377  PLK3
#'  3 60714381 MPIP3
#'  4 60714384  CHK2
#'  5 60714395   P53
#'
#' > rcx[["edges"]]
#'     @id        s        t                           i
#'  1  60714379 60714376 60714377                 neighbor-of
#'  2  60714382 60714381 60714377                 neighbor-of
#'  3  60714385 60714384 60714377                 neighbor-of
#'  4  60714388 60714377 60714376      controls-expression-of
#'  5  60714390 60714377 60714381 controls-phosphorylation-of
#'  6  60714392 60714377 60714381    controls-state-change-of
#'  7  60714393 60714377 60714384 controls-phosphorylation-of
#'  8  60714394 60714377 60714384    controls-state-change-of
#'  9  60714396 60714377 60714395 controls-phosphorylation-of
#'  10 60714398 60714377 60714395    controls-state-change-of
#'  11 60714399 60714377 60714395                 neighbor-of
#' } 
#' 
#' 
#' @param json JSON data
#' @param verbose logical; whether to print out extended feedback 
#' @return returns object of class RCX if successfull, NULL otherwise
#' @seealso \code{\link{ngraph.fromRCX}} \code{\link{ngraph.toRCX}} \code{\link{rcx.toJSON}} 
#' @aliases RCX
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.network(ndexcon,pws[1,"externalId"]) }
#' @export
rcx.fromJSON <- function(json, verbose = FALSE){
  
  if(!jsonlite::validate(json)) {
    warning("rcx.fromJSON: parameter json does not contain valid JSON")
    return(NULL)
  }

  json = jsonlite::fromJSON(json)
  ## this created a data.frame of size n x m, where n = (columns) number of individual aspects and m = (rows) overall number of aspects received
  ## this means: most entries within each column are NULL. if they are not null, they contain a data.frame containing the aspect according to the column name.
  ## note: not all data.frames contained within a column must have matching column, e.g. pre- and post-metdata
  ## implementation: 
  #break up data.frame columns into a list
  #merge pre- and post-metadata
  #consolidate other aspects using rbind which also removes NULLs
  
  jsonlist = as.list(json)
  aspectlist = list()
  
  ### merge pre- and post-metadata. this is special as pre- and post-metadata can have the entries of the same name. 
  sel = which(!sapply(jsonlist[["metaData"]], is.null))
  if(length(sel)==1) {
    aspectlist[["metaData"]] = jsonlist[["metaData"]][[sel[1]]]
  }
  if(length(sel)==2) {
    aspectlist[["metaData"]] = merge(jsonlist[["metaData"]][[sel[1]]],jsonlist[["metaData"]][[sel[2]]],by="name",all = T)
    if("properties.x" %in% names(aspectlist$metaData)) {
      aspectlist[["metaData"]]$properties = aspectlist$metaData$properties.x
      aspectlist[["metaData"]]$properties.x = NULL
      aspectlist[["metaData"]]$properties.y = NULL
    }
  }
  if(!(length(sel) %in% c(1,2))) {
    warning(paste0("JSON2RCX: data contained ",length(sel), " parts of metaData. Must be 1 or 2. Returning NULL." ))
    return(NULL)
  }
  
  ### remaining aspects must have same structure: rbind them
  for(i in names(jsonlist)) {
    if(i == "metaData") next
    aspectlist[[i]] = plyr::rbind.fill(jsonlist[[i]])
      #do.call("rbind", jsonlist[[i]]) might prove too slow for large amount of data
  }
  
  # ### handle core aspects manually, this saves time on the rbind step below - especially for large networks
  # for(i in c("nodes","edges","networkAttributes", "nodeAttributes", "edgeAttributes")) {
  #   if (i %in% names(jsonlist)) {
  #     aspectlist[[i]] = do.call("rbind", jsonlist[[i]])
  #   }
  # } 
  # ### remove NULLs from the lists
  # for(i in names(jsonlist)) {
  #   if(i %in% c("nodes","edges","networkAttributes", "nodeAttributes", "edgeAttributes")) next
  #   sel = !sapply(jsonlist[[i]], is.null)
  #   aspectlist[[i]] = jsonlist[[i]][sel]
  # }
  
  # aspectlist is still a named list of aspects represented in data.frames
    
  #set class
  class(aspectlist) = c("RCX",class(aspectlist))
  return(aspectlist)
}


#' Generate JSON data from RCX object
#' 
#' @param rcx RCX object
#' @param verbose logical; whether to print out extended feedback
#' @param pretty logical; adds indentation whitespace to JSON output
#' @return json jsonlite json object if successfull, NULL otherwise
#' @seealso \code{\link{ngraph.fromRCX}} \code{\link{ngraph.toRCX}} \code{\link{rcx.fromJSON}}
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.network(ndexcon,pws[1,"externalId"]) 
#' rcxjson = rcx.toJSON(rcx) }
#' @export
rcx.toJSON <- function(rcx, verbose = FALSE, pretty = FALSE){
  if(is.null(rcx) || !("RCX" %in% class(rcx))) {
    warning("rcx.toJSON: parameter rcx does not contain RCX object")
    return(NULL)
  }
  
  ## numberVerification has to be 2^48 = 281,474,976,710,655
  rcx$numberVerification = data.frame(longNumber=281474976710655)
  jsonCol = c()
  ## numberVerifiction has to be first!!
  ## metaData has to be second!!
  rcxNames = names(rcx)
  rcxNames = rcxNames[rcxNames!="numberVerification"]
  rcxNames = rcxNames[rcxNames!="metaData"]
  rcxNames = c("numberVerification", "metaData", rcxNames)
  for(aspect in rcxNames){
    ## if any of the aspects has a datatype ('d') property, at least one of the datatypes is not of 'string' (default datatype).
    ## this means, the corresponding values ('v') have to be wrapped in arrays, if they are defined as kind of list (e.g. 'list_of_string', 'list_of_integer',...)
    if(('v' %in% names(rcx[[aspect]]))&&('d' %in% names(rcx[[aspect]]))){
      tmp = rcx[[aspect]]
      tmpList = list()
      tmpNoList = list()
      isListVector = (!is.na(tmp$d)&(substr(tmp[,'d'],1,nchar("list_of_"))=="list_of_"))
      tmpList[[aspect]] = rcx[[aspect]][isListVector,]       # has to be wrapped
      tmpNoList[[aspect]] = rcx[[aspect]][!isListVector,]    # doesn't have to be wrapped
      tmpList[[aspect]]$v = as.list(tmpList[[aspect]]$v)     # forces toJSON to encode the elements as arrays
      
      ## don't add an empty aspect, if all v's are lists
      if(length(tmpNoList)!=0){
        jsonCol = c(jsonCol, jsonlite::toJSON(tmpNoList, pretty = pretty))
      }
      ## don't add an empty aspect, if none v is a list (but might be an integer)
      if(length(tmpList)!=0){
        jsonCol = c(jsonCol, jsonlite::toJSON(tmpList, pretty = pretty))
      }
    }else{
      tmpList = list()
      tmpList[[aspect]]=rcx[[aspect]]
      jsonCol = c(jsonCol, jsonlite::toJSON(tmpList, pretty = pretty))
    }
  }
  return(paste0('[',paste0(jsonCol, collapse=','),']'))
}

####################################################
## Some convenience functions for RCX 
####################################################


#' Remove all interfering NDEx artefacts from RCX object
#' 
#' @param rcx RCX object
#' @return \code{\link{RCX}} object
#' @seealso \code{\link{rcx.fromJSON}} \code{\link{ndex.get.network}}
#' @details After a RCX object is downloaded from an NDEx server, it will contain some aspects that are not present in a newly generated network, i.e. ndexStatus', provenanceHistory' and 'status'.
#' Removing those aspects might be useful in some cases.
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.network(ndexcon,pws[1,"externalId"]) 
#' rcx = rcx.asNewNetwork(rcx)
#' rcxjson = rcx.toJSON(rcx)
#' ndex.create.network(ndexcon, rcxjson) }
#' @export
rcx.asNewNetwork = function(rcx){
  rcx['ndexStatus'] = NULL          # a newly created network doesn't have an ndex-status yet
  rcx['provenanceHistory'] = NULL   # ... also not an provenance history
  rcx['status'] = NULL              # fragment from retrieving the network from the server
  rcx
}

####################################################
## below this line everything is in developement!
## Do not use anything from below!
####################################################

# ndex cx reqirements:
# number verification at first position!
# metadata at second position
# number verification has a special calculated value! But how?
# a status object/aspect is needed



#' Updating the meta-data of an RCX object
#' 
#' @param rcx RCX object 
#' @param mandatoryAspects character vector; Aspects, that are mandatory for a valid RCX object (by default: "nodes")
#' @param countElementsOfAspects character vector; Aspects, that should be counted (i.e. have an entry in elementCount != NA)
#' @param keyValueAspects character vector; Specifies the aspects, that consist of key-value pairs
#' @param verbose logical; whether to print out extended feedback
#' @return \code{\link{RCX}} object
#' @details For a given RCX object the meta-data is updated, i.e. the counted elements and id counter are updated. If an aspect was removed, it will also removed from the meta-data. But, in contrast, if an aspect is added (manually, and not by ndex.RCXaddAspect), no meta-data is added.
#' If mandatory aspects (specified in mandatoryAspects parameter; by default "mandatoryAspects=c('nodes')", which is the minimal requirement for a valid RCX object) are missing in the RCX object, an error is thrown.
#' By default, the aspects, which already have an entry for "elementCount" (i.e. != NA), are updated. If any aspect name is given by the "countElementsOfAspects" parameter, those aspects are used for counting (the default behavour usually corresponds with setting "countElementsOfAspects=c('citations','@context','edges','ndexStatus','nodes')")
#' For counting the elements, there are two different strategies: The default strategy is counting the lines of the data.frame (which translate in JSON to the elements of an array).
#' In some cases (e.g. for the aspect "@context"), the data consists of simple key-value pairs. In the RCX object the keys are the (named) columns, and the values embeded in the single line of the data.frame (which translates in JSON to an array, with an object as single entry).
#' The correct way of counting the elements of those aspects is therefore by column, which can spezified by the "keyValueAspects" parameter, e.g. by default the parameter is set "keyValueAspects=c('@context')" to allow the key-value pairs in the "@context" aspect to be counted correctly.
#' (Note: Since keys are supposed to be unique, this might be the savest way realizing this within both, JSON and R)
#' @examples 
#' \dontrun{
#' rcx = rcx.updateMetaData(rcx)
#' # or with explicitly set default values
#' rcx = rcx.updateMetaData(rcx, mandatoryAspects=c('nodes'), countElementsOfAspects=NULL, keyValueAspects=c('@context'), verbose=FALSE)
#' # which, in the most cases, equals to
#' rcx = rcx.updateMetaData(rcx, mandatoryAspects=c('nodes'), countElementsOfAspects=c('citations','@context','edges','ndexStatus','nodes'), keyValueAspects=c('@context'), verbose=FALSE)}
#' @export
rcx.validate = function(rcx, mandatoryAspects=c('nodes'), countElementsOfAspects=NULL, keyValueAspects=c('@context'), verbose=FALSE){	#!ToDo: Check if it works and all cases are covered
  if(is.null(rcx) || !("RCX" %in% class(rcx))) {
    warning("rcx.updateMetaData: Parameter rcx does not contain RCX object")
    return(NULL)
  }
  
  # id exporting aspects are required to have specified an id counter (max id in the aspect)
  idCounterAspects = c()
  for(a in names(rcx)){
    if('@id' %in% colnames(rcx[[a]])) {
      idCounterAspects = c(idCounterAspects, a)
    }
  }
  
  # get the most current consistency group
  commonConsistencyGroup = max(rcx$metaData['consistencyGroup'])
  
  # Check if the element count should be updated
  # note: the element count attribute is optional
  updateElementCount = F
  # element count will be updated, if any aspect is manually set
  if(!is.null(countElementsOfAspects)){ updateElementCount = T }
  # ..or some element counts are allready set and it should be just updated
  if('elementCount' %in% names(rcx$metaData)){
      updateElementCount = T
  }else{
    # in case it should be updated, but the column doesn't exist yet, it should be created
    if(updateElementCount){
      rcx$metaData$elementCount = NA
    }
  }
  
  maxLastUpdate = max(rcx$metaData$lastUpdate)
 
  
  for(aspect in rcx$metaData$name){
    if(verbose){cat('checking aspect "',aspect,'"...\n', sep = '')}
    
    # is aspect not anymore present in rcx object?
    if(is.null(rcx[[aspect]])){  
      if(aspect %in% mandatoryAspects){
        warning('rcx.updateMetaData: Aspect "',aspect,'" is mandatory for an RCX model, but cannot be found in this RCX object!\nMandatory aspects for all RCX objects: ',paste0('"',mandatoryAspects,'"', collapse = ', '),'\nMandatory aspects specified for rcx.updateMetaData: ',paste0('"',mandatoryAspects,'"', collapse = ', '))
        return(NULL)
      }else{
        rcx$metaData = rcx$metaData[-which(rcx$metaData$name==aspect),]
        if(verbose){cat('\tAspect not found in RCX object! Therefore it was removed from the meta data.\n', sep = '')}
      }
    }else{
      #consistencyGroup elementCount   lastUpdate version idCounter properties
      ## update consistency Groups
      ## check consistency in an other function!!
      # aspectConsistencyGroup = rcx$metaData[rcx$metaData['name'] == aspect,'consistencyGroup']
      # if(aspectConsistencyGroup != commonConsistencyGroup){
      #   rcx$metaData[rcx$metaData['name'] == aspect,'consistencyGroup'] = commonConsistencyGroup
      #   if(verbose){cat('\tconsistency group updated from "',aspectConsistencyGroup,'" to "',commonConsistencyGroup,'"', sep='')}
      # }
      
      # update id counter to the max id in the aspect (if an @id is set)
      aspectIdCounter = rcx$metaData[rcx$metaData['name'] == aspect,'idCounter']
      if(aspect %in% idCounterAspects){
        oldIdCounter = rcx$metaData[rcx$metaData['name'] == aspect,'idCounter']
        newIdCounter = max(rcx[[aspect]][,'@id'])
        rcx$metaData[rcx$metaData['name'] == aspect,'idCounter'] = newIdCounter
        if(verbose && (is.na(oldIdCounter)||(oldIdCounter!=newIdCounter))){cat('\tId counter updated from "',oldIdCounter,'" to "',newIdCounter,'"\n', sep='')}
      }else{
        rcx$metaData[rcx$metaData['name'] == aspect,'idCounter'] = NA
      }
      
      # update element count
      if(updateElementCount){
        oldAspectElementCount = rcx$metaData[rcx$metaData['name'] == aspect,'elementCount']
        if((!is.na(oldAspectElementCount))||(aspect %in% countElementsOfAspects)){
          countByDimension = 1
          if(aspect %in% keyValueAspects){
            countByDimension = 2
          }
          newAspectElementCount = dim(rcx[[aspect]])[countByDimension]
          rcx$metaData[rcx$metaData['name'] == aspect,'elementCount'] = newAspectElementCount
          if(verbose && (is.na(oldAspectElementCount)||(oldAspectElementCount != newAspectElementCount))){cat('\tElement count updated from "',oldAspectElementCount,'" to "',newAspectElementCount,'"\n', sep='')}
        }
      }
    }
  }
  aspectsToSkip = c('metaData', 'numberVerification', 'status')
  aspectsNotInMetaData = setdiff(names(rcx)[-which(names(rcx) %in% aspectsToSkip)], rcx$metaData$name)
  if(verbose && (length(aspectsNotInMetaData)>0)){cat('Some aspects have no meta-data: ',paste0('"',aspectsNotInMetaData,'"', collapse = ', '),'\n(Except the following aspects, which are not supposed to have any meta-data: ',paste0('"',aspectsToSkip,'"', collapse = ', '),')\n', sep='')}
  return(rcx)
}



#' Updating the meta-data of an RCX object
#' 
#' @param rcx RCX object 
#' @param mandatoryAspects character vector; Aspects, that are mandatory for a valid RCX object (by default: "nodes")
#' @param countElementsOfAspects character vector; Aspects, that should be counted (i.e. have an entry in elementCount != NA)
#' @param keyValueAspects character vector; Specifies the aspects, that consist of key-value pairs
#' @param verbose logical; whether to print out extended feedback
#' @return \code{\link{RCX}} object
#' @details For a given RCX object the meta-data is updated, i.e. the counted elements and id counter are updated. If an aspect was removed, it will also removed from the meta-data. But, in contrast, if an aspect is added (manually, and not by ndex.RCXaddAspect), no meta-data is added.
#' If mandatory aspects (specified in mandatoryAspects parameter; by default "mandatoryAspects=c('nodes')", which is the minimal requirement for a valid RCX object) are missing in the RCX object, an error is thrown.
#' By default, the aspects, which already have an entry for "elementCount" (i.e. != NA), are updated. If any aspect name is given by the "countElementsOfAspects" parameter, those aspects are used for counting (the default behavour usually corresponds with setting "countElementsOfAspects=c('citations','@context','edges','ndexStatus','nodes')")
#' For counting the elements, there are two different strategies: The default strategy is counting the lines of the data.frame (which translate in JSON to the elements of an array).
#' In some cases (e.g. for the aspect "@context"), the data consists of simple key-value pairs. In the RCX object the keys are the (named) columns, and the values embeded in the single line of the data.frame (which translates in JSON to an array, with an object as single entry).
#' The correct way of counting the elements of those aspects is therefore by column, which can spezified by the "keyValueAspects" parameter, e.g. by default the parameter is set "keyValueAspects=c('@context')" to allow the key-value pairs in the "@context" aspect to be counted correctly.
#' (Note: Since keys are supposed to be unique, this might be the savest way realizing this within both, JSON and R)
#' @examples 
#' \dontrun{
#' rcx = rcx.updateMetaData(rcx)
#' # or with explicitly set default values
#' rcx = rcx.updateMetaData(rcx, mandatoryAspects=c('nodes'), countElementsOfAspects=NULL, keyValueAspects=c('@context'), verbose=FALSE)
#' # which, in the most cases, equals to
#' rcx = rcx.updateMetaData(rcx, mandatoryAspects=c('nodes'), countElementsOfAspects=c('citations','@context','edges','ndexStatus','nodes'), keyValueAspects=c('@context'), verbose=FALSE)}
#' @export
rcx.updateMetaData = function(rcx, mandatoryAspects=c('nodes'), countElementsOfAspects=NULL, keyValueAspects=c('@context'), verbose=FALSE){		#!ToDo: Check if it works and all cases are covered
  if(is.null(rcx) || !("RCX" %in% class(rcx))) {
    warning("rcx.updateMetaData: Parameter rcx does not contain RCX object")
    return(NULL)
  }
  
  # id exporting aspects are required to have specified an id counter (max id in the aspect)
  idCounterAspects = c()
  for(a in names(rcx)){
    if('@id' %in% colnames(rcx[[a]])) {
      idCounterAspects = c(idCounterAspects, a)
    }
  }
  
  # get the most current consistency group
  commonConsistencyGroup = max(rcx$metaData['consistencyGroup'])
  
  # Check if the element count should be updated
  # note: the element count attribute is optional
  updateElementCount = F
  # element count will be updated, if any aspect is manually set
  if(!is.null(countElementsOfAspects)){ updateElementCount = T }
  # ..or some element counts are allready set and it should be just updated
  if('elementCount' %in% names(rcx$metaData)){
    updateElementCount = T
  }else{
    # in case it should be updated, but the column doesn't exist yet, it should be created
    if(updateElementCount){
      rcx$metaData$elementCount = NA
    }
  }
  
  maxLastUpdate = max(rcx$metaData$lastUpdate)
  
  
  for(aspect in rcx$metaData$name){
    if(verbose){cat('checking aspect "',aspect,'"...\n', sep = '')}
    
    # is aspect not anymore present in rcx object?
    if(is.null(rcx[[aspect]])){  
      if(aspect %in% mandatoryAspects){
        warning('rcx.updateMetaData: Aspect "',aspect,'" is mandatory for an RCX model, but cannot be found in this RCX object!\nMandatory aspects for all RCX objects: ',paste0('"',mandatoryAspects,'"', collapse = ', '),'\nMandatory aspects specified for rcx.updateMetaData: ',paste0('"',mandatoryAspects,'"', collapse = ', '))
        return(NULL)
      }else{
        rcx$metaData = rcx$metaData[-which(rcx$metaData$name==aspect),]
        if(verbose){cat('\tAspect not found in RCX object! Therefore it was removed from the meta data.\n', sep = '')}
      }
    }else{
      #consistencyGroup elementCount   lastUpdate version idCounter properties
      ## update consistency Groups
      ## check consistency in an other function!!
      # aspectConsistencyGroup = rcx$metaData[rcx$metaData['name'] == aspect,'consistencyGroup']
      # if(aspectConsistencyGroup != commonConsistencyGroup){
      #   rcx$metaData[rcx$metaData['name'] == aspect,'consistencyGroup'] = commonConsistencyGroup
      #   if(verbose){cat('\tconsistency group updated from "',aspectConsistencyGroup,'" to "',commonConsistencyGroup,'"', sep='')}
      # }
      
      # update id counter to the max id in the aspect (if an @id is set)
      aspectIdCounter = rcx$metaData[rcx$metaData['name'] == aspect,'idCounter']
      if(aspect %in% idCounterAspects){
        oldIdCounter = rcx$metaData[rcx$metaData['name'] == aspect,'idCounter']
        newIdCounter = max(rcx[[aspect]][,'@id'])
        rcx$metaData[rcx$metaData['name'] == aspect,'idCounter'] = newIdCounter
        if(verbose && (is.na(oldIdCounter)||(oldIdCounter!=newIdCounter))){cat('\tId counter updated from "',oldIdCounter,'" to "',newIdCounter,'"\n', sep='')}
      }else{
        rcx$metaData[rcx$metaData['name'] == aspect,'idCounter'] = NA
      }
      
      # update element count
      if(updateElementCount){
        oldAspectElementCount = rcx$metaData[rcx$metaData['name'] == aspect,'elementCount']
        if((!is.na(oldAspectElementCount))||(aspect %in% countElementsOfAspects)){
          countByDimension = 1
          if(aspect %in% keyValueAspects){
            countByDimension = 2
          }
          newAspectElementCount = dim(rcx[[aspect]])[countByDimension]
          rcx$metaData[rcx$metaData['name'] == aspect,'elementCount'] = newAspectElementCount
          if(verbose && (is.na(oldAspectElementCount)||(oldAspectElementCount != newAspectElementCount))){cat('\tElement count updated from "',oldAspectElementCount,'" to "',newAspectElementCount,'"\n', sep='')}
        }
      }
    }
  }
  aspectsToSkip = c('metaData', 'numberVerification', 'status')
  aspectsNotInMetaData = setdiff(names(rcx)[-which(names(rcx) %in% aspectsToSkip)], rcx$metaData$name)
  if(verbose && (length(aspectsNotInMetaData)>0)){cat('Some aspects have no meta-data: ',paste0('"',aspectsNotInMetaData,'"', collapse = ', '),'\n(Except the following aspects, which are not supposed to have any meta-data: ',paste0('"',aspectsToSkip,'"', collapse = ', '),')\n', sep='')}
  return(rcx)
}


#' Create a blank rcx object
#' @param nodes At least one node is necessary for be a valid rcx
#' @return RCX object
#' @details Constructor for creating a blank rcx object, that fulfills the minimal requirements of CX
#' @export
rcx.new = function(nodes){	#!ToDo: Implement
	rcx <- list(rcx, ...)
}

#' merging two or more rcx objects
#' @param rcx RCX object
#' @param ... one or more RCX objects
#' @return RCX object
#' @details Two or more RCX objects are merged
#' @export
rcx.merge = function(rcx,...){	#!ToDo: Implement
	rcx <- list(rcx, ...)
}

