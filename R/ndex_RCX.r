##Authors:
#   Frank Kramer [frank.kramer@med.uni-goettingen.de]
## Created: 20 Sep 2016
## Base functions to create, parse, modify CX networks from/to JSON data


#' Create RCX object from JSON data
#' 
#' This function creates an RCX object from a supplied JSON-encoded CX object. It is usually called from within \code{\link{ndex.get.complete.network}}.
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
#' @seealso \code{\link{ndex.RCX2ngraph}} \code{\link{ndex.ngraph2RCX}} \code{\link{ndex.RCX2JSON}} 
#' @aliases RCX
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.complete.network(ndexcon,pws[1,"externalId"]) }
#' @export
ndex.JSON2RCX <- function(json, verbose = FALSE){
  
  if(!jsonlite::validate(json)) {
    warning("ndex.JSON2RCX: parameter json does not contain valid JSON")
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
#' @return json jsonlite json object if successfull, NULL otherwise
#' @seealso \code{\link{ndex.RCX2ngraph}} \code{\link{ndex.ngraph2RCX}} \code{\link{ndex.JSON2RCX}}
#' @examples 
#' \dontrun{
#' ndexcon = ndex.connect(verbose=T)
#' pws = ndex.find.networks(ndexcon,"p53")
#' rcx = ndex.get.complete.network(ndexcon,pws[1,"externalId"]) 
#' rcxjson = ndex.RCX2JSON(rcx) }
#' @export
ndex.RCX2JSON <- function(rcx, verbose = FALSE){
  
  if(is.null(rcx) || !("RCX" %in% class(rcx))) {
    warning("ndex.RCX2JSON: parameter rcx does not contain RCX object")
    return(NULL)
  }
  
  return(jsonlite::toJSON(rcx, pretty=T))
}



