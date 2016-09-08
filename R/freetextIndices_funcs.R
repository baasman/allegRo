### FREE TEXT INDICES

#' listFreeTextIndices
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#'
#' @return A matrix of all active free text indices
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' listFreeTextIndices(service,catalogid = "root",repo = 'testRepo')
#' }
listFreeTextIndices = function(service,catalogid = "root",repo = ""){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/freetext/indices")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/freetext/indices")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' createFreeTextIndex
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param index Name of index of interest
#' @param predicates Can be a list. Indicates the predicates that should be indexed. When not given, all predicates are indexed.
#' @param indexLiterals A boolean (defaults to true) that determines whether literal are indexed.
#' @param indexResources Can be given the values true, false, or short. Default is false. short means to index only the part of the resource after the last # or / character.
#' @param indexFields ...
#' @param minimumWordSize ...
#' @param stopWord Can be a list.  Determines the set of stop-words, words that are not indexed. Defaults to a small set of common English words. To override this default and specify that no stop-words should be used, pass this parameter once, with an empty list().
#' @param wordFilter Can be a list. Specify a word filter, which is an operation applied to words before they are indexed and before they are searched for. Used to 'normalize' words.
#' @param innerChars ...
#' @param borderChars ...
#' @param tokenizer ...
#'
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' createFreeTextIndex(service,repo = "testRepo",index = 'index',
#' predicates = "<p>",stopWord = list("and","it"))
#' }
createFreeTextIndex = function(service, catalogid = "root", repo = "", index, predicates=NULL, indexLiterals=NULL, indexResources=FALSE,
                    indexFields=NULL, minimumWordSize=NULL, stopWord=NULL, wordFilter=NULL,
                    innerChars=NULL, borderChars=NULL, tokenizer=NULL){

  queryargs = convertLogical(expandUrlArgs(list(predicates = predicates, indexLiterals = indexLiterals, indexResources = indexResources,
                                          indexFields = indexFields, minimumWordSize = minimumWordSize,stopWord = stopWord,
                                          wordFilter = wordFilter, innerChars = innerChars, borderChars = borderChars,
                                          tokenizer = tokenizer)))
  body = NULL

  if(length(stopWord) == 0) stopWord = ""
  if(length(indexFields) == 0) indexFields = ""

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/freetext/indices/",index)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/freetext/indices/",index)
  }

  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body))
}


#' getFreeTextIndexConfiguration
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param index The index you want to see configurations for
#'
#' @return A list describing all configuration parametes of index of interest
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' createFreeTextIndex(service,repo = "testRepo",index = 'index')
#' getFreeTextIndexConfiguration(service,catalogid = "root",repo = 'testRepo',index = "index")
#' }
getFreeTextIndexConfiguration = function(service,catalogid = "root",repo = "",index){

  if(missing(index)){
    cat("No index entered. These are the following free-text-indices currently present: \n \n")
    return(listFreeTextIndices(service,catalogid = catalogid,repo = repo))
  }

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/freetext/indices/",index)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/freetext/indices/",index)
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}




#' evalFreeTextSearch
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param pattern A string to match triples on
#' @param index The index you want to see configurations for
#' @param limit Number of triples to return
#' @param offset Number of triples to skip
#' @param sorted Boolean, defaults to FALSE
#'
#' @return A list describing all configuration parametes of index of interest
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' createFreeTextIndex(service,repo = "testRepo",index = 'index')
#' getFreeTextIndexConfiguration(service,catalogid = "root",repo = 'testRepo',index = "index")
#' }
evalFreeTextSearch = function(service,catalogid = "root",repo = "",pattern,index = NULL,
                              limit = NULL,offset = NULL,sorted = NULL){

  queryargs = convertLogical(expandUrlArgs(list(pattern = pattern, limit = limit, offset = offset,index = index, sorted = sorted)))
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/freetext")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/freetext")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}


