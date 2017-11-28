################################################################################
## Authors:
##   Frank Kramer [frank.kramer@med.uni-goettingen.de]
##   Florian Auer [florian.auer@med.uni-goettingen.de]
##   Zaynab Hammoud [zaynab.hammoud@med.uni-goettingen.de]
##
## History:
##   Created on 20 September 2016 by Kramer
##   Restructured on 10 January 2017 by Auer
##
## Description:
##    Base functions to create, parse, modify CX networks from/to JSON data
################################################################################


####################################################
## Conversion from/to JSON
####################################################

#' Create RCX object from JSON data
#'
#' This function creates an RCX object from a supplied JSON-encoded CX object.
#' RCX objects store the CX data as a named list of data.frames containing metaData and all aspects of the network.
#'
#' The structure of an RCX object, as shown via str(rcx) could be a list like this:\cr
#' \preformatted{
#' > str(rcx)
#'
#' List of 12
#' $ metaData          :'data.frame':    11 obs. of  7 variables:
#'   ..$ name            : chr [1:11] "citations" "@context" "edgeAttributes" "edgeCitations" ...
#'   ..$ consistencyGroup: int [1:11] 1 1 1 1 1 1 1 1 1 1 ...
#'   ..$ elementCount    : int [1:11] 4 23 NA NA 11 1 NA NA NA 5 ...
#'   ..$ lastUpdate      : num [1:11] 1.44e+12 1.44e+12 1.44e+12 1.44e+12 1.44e+12 ...
#'   ..$ version         : chr [1:11] "1.0" "1.0" "1.0" "1.0" ...
#'   ..$ idCounter       : int [1:11] 60714397 NA NA NA 60714399 NA NA NA NA 60714395 ...
#'   ..$ properties      :List of 11
#' $ numberVerification:'data.frame':    1 obs. of  1 variable:
#'   ..$ longNumber: num 2.81e+14
#' $ ndexStatus        :'data.frame':    1 obs. of  10 variables:
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
#' $ @context          :'data.frame':    1 obs. of  23 variables:
#'   ..$ GENPEPT                      : chr "http://www.ncbi.nlm.nih.gov/protein/"
#'   ..$ NCBI GENE                    : chr "http://identifiers.org/ncbigene/"
#'   ..$ ENSEMBL                      : chr "http://identifiers.org/ensembl/"
#'   [...]
#' $ networkAttributes :'data.frame':    4 obs. of  2 variables:
#'   ..$ n: chr [1:4] "name" "description" "version" "ndex:sourceFormat"
#'   ..$ v: chr [1:4] "PLK3 signaling events" "This network ..." [...]
#' $ citations         :'data.frame':    4 obs. of  7 variables:
#'  ..$ @id           : int [1:4] 60714380 60714383 60714386 60714397
#'  ..$ dc:identifier : chr [1:4] "pmid:17264206" "pmid:14968113" "pmid:12242661" "pmid:11551930"
#'  ..$ dc:type       : chr [1:4] "URI" "URI" "URI" "URI"
#'  ..$ attributes    :List of 4 [...]
#' $ nodes             :'data.frame':    5 obs. of  2 variables:
#'  ..$ @id: int [1:5] 60714376 60714377 60714381 60714384 60714395
#'  ..$ n  : chr [1:5] "CCNE1" "PLK3" "MPIP3" "CHK2" ...
#' $ nodeAttributes    :'data.frame':    10 obs. of  4 variables:
#'   ..$ po: int [1:10] 60714376 60714376 60714377 60714377 60714381 60714381 60714384 60714384 60714395 60714395
#'   ..$ n : chr [1:10] "alias" "relatedTo" "alias" "relatedTo" ...
#'   ..$ v :List of 10
#'     .. ..$ : chr [1:6] "UniProt Knowledgebase:Q92501" "UniProt Knowledgebase:Q9UD21"  ...
#'     .. ..$ : chr [1:98] "GENE ONTOLOGY:GO:0003713" "GENE ONTOLOGY:GO:0005515"  ...
#'     [...]
#'   ..$ d : chr [1:10] "list_of_string"  ...
#' $ edges             :'data.frame':    11 obs. of  4 variables:
#'   ..$ @id: int [1:11] 60714379 60714382  ...
#'   ..$ s  : int [1:11] 60714376 60714381  ...
#'   ..$ t  : int [1:11] 60714377 60714377  ...
#'   ..$ i  : chr [1:11] "neighbor-of" "neighbor-of"  ...
#' $ edgeCitations     :'data.frame':    11 obs. of  2 variables:
#'   ..$ po       :List of 11
#'   .. ..$ : int 60714379
#'   .. ..$ : int 60714382
#'   [...]
#' ..$ citations:List of 11
#' .. ..$ : int 60714380
#' .. ..$ : int 60714383
#'   [...]
#' $ status            :'data.frame':    1 obs. of  2 variables:
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
#'
#' @return returns object of class RCX if successfull, NULL otherwise
#'
#' @seealso \code{\link{rcxgraph_fromRCX}} \code{\link{rcxgraph_toRCX}} \code{\link{rcx_toJSON}}
#' @aliases RCX
#'
#' @examples
#' ## Create an RCX object
#' rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))
#' ## Convert to JSON
#' json = rcx_toJSON(rcx)
#' ## Convert it back from JSON
#' rcx = rcx_fromJSON(json)
#' @export
rcx_fromJSON <- function(json, verbose = FALSE){
  #!TODO: add a better way for datatype columns ("d") of properties and nodeAttributes, edgeAttributes, etc.
  if(!jsonlite::validate(json)) {
    stop("rcx_fromJSON: parameter json does not contain valid JSON")
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
    aspectlist[["metaData"]] = merge(jsonlist[["metaData"]][[sel[1]]],jsonlist[["metaData"]][[sel[2]]],by="name",all = TRUE)
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
  }
  
  #set class
  class(aspectlist) = c("RCX",class(aspectlist))
  return(aspectlist)
}


#' Generate JSON data from RCX object
#'
#' @param rcx RCX object
#' @param verbose logical; whether to print out extended feedback
#' @param pretty logical; adds indentation whitespace to JSON output
#'
#' @return json jsonlite json object if successfull, NULL otherwise
#' @seealso \code{\link{rcxgraph_fromRCX}} \code{\link{rcxgraph_toRCX}} \code{\link{rcx_fromJSON}}
#'
#' @examples
#' ## Create an RCX object
#' rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))
#' ## Convert to JSON
#' json = rcx_toJSON(rcx)
#' @export
rcx_toJSON <- function(rcx, verbose = FALSE, pretty = FALSE){
  if(is.null(rcx) || !("RCX" %in% class(rcx))) {
    stop("rcx_toJSON: parameter rcx does not contain RCX object")
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
      jsonCol = c(jsonCol,paste0('{"',aspect,'":',rcx_aspect_toJSON(rcx[[aspect]], verbose, pretty),'}'))
  }
  return(paste0('[',paste0(jsonCol, collapse=','),']'))
}


#' Generate JSON data for a single aspect of a RCX object
#'
#' @param rcxAspect aspect in RCX object (rcx[[aspectName]])
#' @param verbose logical; whether to print out extended feedback
#' @param pretty logical; adds indentation whitespace to JSON output
#'
#' @return json object if successfull, empty string otherwise
#' @seealso \code{\link{rcx_toJSON}} and \code{\link{rcx_fromJSON}}
#'
#' @examples
#' ## Create an RCX object
#' rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))
#' ## Convert RCX aspect to JSON
#' rcxNodesJson = ndexr:::rcx_aspect_toJSON(rcx$nodes)
rcx_aspect_toJSON <- function(rcxAspect, verbose = FALSE, pretty = FALSE){
    result = ''
    ## if any of the aspects has a datatype ('d') property, at least one of the datatypes is not of 'string' (default datatype).
    ## this means, the corresponding values ('v') have to be wrapped in arrays, if they are defined as kind of list (e.g. 'list_of_string', 'list_of_integer',...)
    if(('v' %in% names(rcxAspect))&&('d' %in% names(rcxAspect))){
        tmp = rcxAspect
        isListVector = (!is.na(tmp$d)&(substr(tmp[,'d'],1,nchar("list_of_"))=="list_of_"))
        tmpList = rcxAspect[isListVector,]       # has to be wrapped
        tmpNoList = rcxAspect[!isListVector,]    # doesn't have to be wrapped
        tmpList$v = as.list(tmpList$v)              # forces toJSON to encode the elements as arrays
        jsonCol = c()
        ## don't add an empty aspect, if all v's are lists
        if(dim(tmpNoList)[1]!=0){
            tmpNoList$v <- unlist(tmpNoList$v)
#            tmpTxt = jsonlite::toJSON(tmpNoList, na='string', pretty = pretty) # TODO!! [fauer:20.03.2017]: NA as string or NULL? for citations$`dc:contributor` it has to be NULL!
            tmpTxt = jsonlite::toJSON(tmpNoList, na=NULL, pretty = pretty)
            tmpTxt = sub('\n*$','',substr(tmpTxt,2,nchar(tmpTxt)-1))
            jsonCol = c(jsonCol, tmpTxt)
        }

        ## don't add an empty aspect, if none v is a list (but might be an integer)
        if(dim(tmpList)[1]!=0){
#            tmpTxt = jsonlite::toJSON(tmpList, na='string', pretty = pretty)
            tmpTxt = jsonlite::toJSON(tmpList, na=NULL, pretty = pretty)
            tmpTxt = substr(tmpTxt,2,nchar(tmpTxt)-1)
            jsonCol = c(jsonCol, tmpTxt)
        }
        
        result = paste0('[',paste0(jsonCol, collapse=','),']')
    }else{
        tmpList = rcxAspect
#        result = jsonlite::toJSON(tmpList, na='string', pretty = pretty)
        result = jsonlite::toJSON(tmpList, na=NULL, pretty = pretty)
    }

    return(result)
}

####################################################
## Some convenience functions for RCX
####################################################


#' Remove all interfering NDEx artefacts from RCX object
#'
#' @param rcx RCX object
#'
#' @return \code{\link{RCX}} object
#' @seealso \code{\link{rcx_fromJSON}}
#'
#' @details After a RCX object is downloaded from an NDEx server, it will contain some aspects that are not present in a newly generated network, i.e. ndexStatus', provenanceHistory' and 'status'.
#' Removing those aspects might be useful in some cases.
#'
#' @examples
#' ## Create an RCX object
#' rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))
#' ## Remove NDEx artefacts
#' rcx = rcx_asNewNetwork(rcx)
#' \dontrun{
#' rcxjson = rcx_toJSON(rcx)
#' ndex_create_network(ndexcon, rcxjson)
#' }
#' @export
rcx_asNewNetwork = function(rcx){
  rcx['ndexStatus'] = NULL          # a newly created network doesn't have an ndex-status yet
  rcx['provenanceHistory'] = NULL   # ... also not an provenance history
  rcx['status'] = NULL              # fragment from retrieving the network from the server
  return(rcx)
}

#' Create a blank rcx object
#'
#' This function generates a (blank) RCX object. For a valid RCX, at least one node has to be specified. Optional attributes are 'n' for names and 'r' for represents.
#' Ids have to be unique (in nodes) and may not contain 'NA' values. For names and represents attributes, 'NA' values are allowed.
#'
#' @param nodes vector or data.frame (default: c('@id'=1) ) node(s)
#'
#' @return RCX object
#'
#' @examples
#' rcx = rcx_new()
#' rcx = rcx_new(c('@id'=1))                                #same as one before
#' rcx = rcx_new(nodes=c('@id'=1))                          #same as one before
#' rcx = rcx_new(data.frame('@id'=c(1), check.names=FALSE))     #same as one before
#' rcx = rcx_new(c('@id'=1, n='Some Name'))
#' rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))
#' rcx = rcx_new(data.frame('@id'=c(1),n=c('Some Name'), r=c('HGNC:Symbol'), check.names=FALSE))    #same as one before
#' rcx = rcx_new(data.frame('@id'=c(1,2,3),n=c('Some Name','And another name',NA), r=c('HGNC:Symbol',NA,'UniProt:C3P0'), check.names=FALSE))
#' @export
rcx_new = function(nodes=c('@id'=1)){
# TODO : add parameters for edges and other core aspects
# rcx_new = function(nodes=c('@id'=1), edges, nodeAttributes, edgeAttributes, networkAttributes){
    if(is.null(nodes)) stop('rcx_new: At least one node is necessary!')
    if(!'@id' %in% names(nodes)) stop('rcx_new: No "@id" column in nodes!')
    ids = as.character(nodes[['@id']])
    if(length(unique(ids)) != length(ids)) stop('rcx_new: Some ids in "@id" column are duplicated!')
    if(any(NA %in% nodes[['@id']])) stop('rcx_new: Some ids in "@id" column have "NA" value!')
    rcx <- list(nodes=data.frame('@id'=ids, stringsAsFactors=FALSE, check.names=FALSE))
    if('n' %in% names(nodes)) rcx$nodes$n = as.character(nodes[['n']])
    if('r' %in% names(nodes)) rcx$nodes$r = as.character(nodes[['r']])
    class(rcx) = c("RCX",class(rcx))
    rcx = rcx_updateMetaData(rcx)
    return(rcx)
}


#' Updating the meta-data of an RCX object
#'
#' @param rcx RCX object
#' @param mandatoryAspects character vector; Aspects, that are mandatory for a valid RCX object (by default: "nodes")
#' @param excludeAspects character vector; Aspects, that are excluded for generating metaData (by default: "metaData", "numberVerification" and "status")
#' @param force logical; force the creation of new metaData (even if the RCX object already contains metaData)
#' @param verbose logical; whether to print out extended feedback
#'
#' @return \code{\link{RCX}} object
#'
#' @details For a given RCX object the meta-data is updated, i.e. the counted elements and id counter are updated. If an aspect was added/removed, it will also added/removed from the meta-data.
#' If mandatory aspects (specified in mandatoryAspects parameter) are missing in the RCX object, an error is thrown.
#'
#' @examples
#' ## Create an RCX object
#' rcx = rcx_new(c('@id'=1, n='Some Name', r='HGNC:Symbol'))
#' ## update meta-data
#' rcx = rcx_updateMetaData(rcx)
#' # or with explicitly set default values
#' rcx = rcx_updateMetaData(rcx, mandatoryAspects=c('nodes'), excludeAspects=c("metaData", "numberVerification", "status"), force=FALSE, verbose=FALSE)
#' @export
rcx_updateMetaData = function(rcx, mandatoryAspects=c('nodes'), excludeAspects=c("metaData", "numberVerification", "status"), force=FALSE, verbose=FALSE){
    if(missing(rcx) || is.null(rcx) || !("RCX" %in% class(rcx))) stop("rcx_updateMetaData: Parameter rcx does not contain RCX object")

    # check if mandatoryAspects are present in the RCX object
    if(any(!(mandatoryAspects %in% names(rcx)))) stop(paste0("rcx_updateMetaData: Mandatory aspects are missing in the RCX object: ", paste0(mandatoryAspects[!(mandatoryAspects %in% names(rcx))], collapse=', ')))

    # get meta data from RCX object
    # create it, if it doesn't exist (or if it is forced)
    metaData = rcx$metaData
    if(is.null(metaData) || force) {
        if(verbose) print('rcx_updateMetaData: MetaData will be created')
        metaData = data.frame(consistencyGroup=1, elementCount=0, lastUpdate=1, name = sort(names(rcx)[!(names(rcx) %in% excludeAspects)]), version='1.0', idCounter=NA, stringsAsFactors = FALSE)
    }else{
        if(verbose) print('rcx_updateMetaData: Existing metaData will be updated')
        
        if(!('consistencyGroup' %in% names(metaData))){
            metaData$consistencyGroup = 1
            if(verbose) warning('rcx_updateMetaData: No consistancy groups specified (Set by default to "1")!  Check manually for consistency!')
        }
        if(!('elementCount' %in% names(metaData))) metaData$elementCount = 0
        if(!('lastUpdate' %in% names(metaData))) metaData$lastUpdate = 1
        if(!('version' %in% names(metaData))) metaData$version = '1.0'
        if(!('idCounter' %in% names(metaData))) metaData$idCounter = NA
        properties = NULL
        if('properties' %in% names(metaData)){
            properties = metaData$properties
            metaData$properties = NULL
        }else{
            properties = rep(list(), length.out=dim(metaData)[1])
        }

        aspects = names(rcx)[!(names(rcx) %in% excludeAspects)]
        aspectsAlreadyInMDIndex = which(metaData$name %in% aspects)

        # exclude rows, that are no aspects and order columns
        metaData = metaData[aspectsAlreadyInMDIndex,sort(colnames(metaData))]
        properties = properties[aspectsAlreadyInMDIndex]

        # find aspects missing in meta-data
        aspectsNotYetInMD = aspects[!aspects %in% metaData$name]

        # add missing aspects to metaData
        if(length(aspectsNotYetInMD)!=0) {
            metaData = rbind(metaData, data.frame(consistencyGroup=1, elementCount=0, idCounter=NA, lastUpdate=1, name = aspectsNotYetInMD, version='1.0', stringsAsFactors = FALSE))
            properties = c(properties,rep(list(),length.out=length(aspectsNotYetInMD)))
        }

        # add the properties column again
        metaData$properties = properties
    }

    if(verbose) print('rcx_updateMetaData: idCounter will be updated')
    # id exporting aspects are required to have specified an id counter (max id in the aspect)
    metaData$idCounter = sapply(metaData$name, function(x){return(ifelse('@id' %in% colnames(rcx[[x]]), max(rcx[[x]]$'@id'), NA))})

    if(verbose) print('rcx_updateMetaData: Consistency groups will be checked')
    # get the current consistency group(s)
    consistencyGroups = unique(metaData$consistencyGroup)
    if(verbose && (length(consistencyGroups)>1)){
        warn = paste0("rcx_updateMetaData: RCX object contains ",length(consistencyGroups), " consistency groups! Check the groups manually for consistency!\n")
        for(cgNr in consistencyGroups){
            warn = paste0(warn, 'Group ',cgNr, ': ', paste0(metaData$name[metaData$consistencyGroup==cgNr], collapse = ', '),'\n')
        }
        warning(warn)
    }

    # update element count
    metaData$elementCount = sapply(metaData$name, function(x){return(max(dim(rcx[[x]])[1],0))})    # max(0,NULL) = 0 to set default value to 0

    # set properties (if not already set)
    if(!('list' %in% class(metaData$properties))){
        metaData$properties=rep(list(), length.out=dim(metaData)[1])
    }

    # order meta-data elements by names and correct the row numbering
    metaData = metaData[order(metaData$name),order(colnames(metaData))]
    rownames(metaData) = 1:dim(metaData)[1]

    rcx$metaData = metaData
    return(rcx)
}


####################################################
###
###   Print functions
###
####################################################

#' Print a RCX object
#'
#' @param x RCX object; stores the CX data as a named list of data.frames containing metaData and all aspects of the network.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Just prints the RCX object
#'
#' @examples
#' rcx = rcx_new(data.frame('@id'=c(1,2,3),n=c('Some Name','And another name',NA), r=c('HGNC:Symbol',NA,'UniProt:C3P0'), check.names=FALSE))
#' print(rcx)
#' @seealso  \code{\link{rcx_fromJSON}} and \code{\link{rcx_new}}
#' @export
print.RCX <- function(x,...){
    cat('RCX object containing the following aspects:\n')
    tmpColNames = colnames(x$metaData)
    tmpColNames = tmpColNames[order(tmpColNames)]
    tmpColNames = c('name',tmpColNames[tmpColNames!='name'])
    print(x$metaData[tmpColNames])
    tmpNames = names(x)
    tmpAspects = x$metaData$name
    metaAspects = tmpNames[!tmpNames %in% tmpAspects]
    if(length(metaAspects)!=0){
        cat('\nAdditionally the following aspects contain meta-data:\n')
        print(metaAspects)
    }
    invisible(x)
}

