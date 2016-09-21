##Authors:
#   Frank Kramer [frank.kramer@med.uni-goettingen.de]
## Created: 20 Sep 2016
## Base functions to create, parse, modify CX networks from/to JSON data

#############
# RCX object:
# list (
#  
# )

#' Create RCX object from JSON data
#' 
#' @param json JSON data
#' @param verbose logical; whether to print out extended feedback 
#' @return returns object of class RCX if successfull, NULL otherwise
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
  
  ### merge pre- and post-metadata
  sel = which(!sapply(jsonlist[["metaData"]], is.null))
  if(length(sel)==1) {
    aspectlist$metaData[[1]] = jsonlist[["metaData"]][[sel[1]]]
  }
  if(length(sel)==2) {
    aspectlist$metaData[[1]] = merge(jsonlist[["metaData"]][[sel[1]]],jsonlist[["metaData"]][[sel[2]]],by="name",all = T)
    if("properties.x" %in% names(aspectlist$metaData[[1]])) {
      aspectlist$metaData[[1]]$properties = aspectlist$metaData[[1]]$properties.x
      aspectlist$metaData[[1]]$properties.x = NULL
      aspectlist$metaData[[1]]$properties.y = NULL
    }
  }
  if(!(length(sel) %in% c(1,2))) {
    warning(paste0("JSON2RCX: data contained ",length(sel), " parts of metaData. Must be 1 or 2. Returning NULL." ))
    return(NULL)
  }
  
  ### remaining aspects must have same structure: rbind them
  for(i in names(jsonlist)) {
    if(i == "metaData") next
    aspectlist[[i]] = do.call("rbind", jsonlist[[i]])
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
#' @export
ndex.RCX2JSON <- function(rcx, verbose = FALSE){
  return(jsonlite::toJSON(rcx))
}



