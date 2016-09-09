### FREE TEXT INDICES

#' List free-text indices
#'
#' @description Return a list of freetext indices defined on the store
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
#' service = createService('localhost','user','password')
#' listFreeTextIndices(service,catalogid = 'root',repo = 'testRepo')
#' }
listFreeTextIndices = function(service, catalogid = "root", repo = "") {

  queryargs = NULL
  body = NULL

  if (catalogid == "root") {
    url = paste0(service$url, "repositories/", repo, "/freetext/indices")
  } else {
    url = paste0(service$url, "catalogs/", catalogid, "/repositories/",
                 repo, "/freetext/indices")
  }

  return(ag_get(service = service, url = url, queryargs = queryargs,
                body = body))
}

#' Create a free-text index
#'
#' @description Create or modify a freetext-index. I suggest reading the official documentation at
#' http://franz.com/agraph/support/documentation/current/text-index.html
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param indexName Name of index of interest
#' @param predicate Can be a list. Indicates the predicates that should be indexed. When not given, all predicates are indexed.
#' @param indexLiterals A boolean (defaults to true) that determines whether literal are indexed.
#' @param indexResources Can be given the values true, false, or short. Default is false. short means to index only the part of the resource after the last # or / character.
#' @param indexFields ...
#' @param minimumWordSize ...
#' @param stopWords Can be a list.  Determines the set of stop-words, words that are not indexed. Defaults to a small set of common English words. To override this default and specify that no stop-words should be used, pass this parameter once, with an empty list().
#' @param wordFilter Can be a list. Specify a word filter, which is an operation applied to words before they are indexed and before they are searched for. Used to 'normalize' words.
#' @param innerChars ...
#' @param borderChars ...
#' @param tokenizer ...
#' @param reIndex Defaults to TRUE. Should old triples be re-indexed
#'
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService('localhost','user','password')
#' createFreeTextIndex(service,repo = 'testRepo',indexName = 'index',
#' predicate = '<p>',stopWords = list('and','it'))
#' }
createFreeTextIndex = function(service, catalogid = "root", repo = "",
                               indexName, predicate, indexLiterals = NULL, indexResources = FALSE,
                               indexFields = NULL, minimumWordSize = NULL, stopWords = NULL, wordFilter = NULL,
                               innerChars = NULL, borderChars = NULL, tokenizer = NULL,reIndex = NULL) {

  il = NULL
  if(!missing(indexLiterals)) il = TRUE
  queryargs = convertLogical(expandUrlArgs(list(predicate = predicate,
                                                indexLiterals = il, indexLiteralType = indexLiterals, indexResources = indexResources,
                                                indexField = indexFields, minimumWordSize = minimumWordSize, stopWord = stopWords,
                                                wordFilter = wordFilter, innerChars = innerChars, borderChars = borderChars,
                                                tokenizer = tokenizer,reIndex = reIndex)))
  body = NULL

  if (is.list(stopWords) & length(stopWords) == 0)
    stopWord = ""
  if (is.list(indexFields) & length(indexFields) == 0)
    indexFields = ""

  if (catalogid == "root") {
    url = paste0(service$url, "repositories/", repo, "/freetext/indices/",
                 indexName)
  } else {
    url = paste0(service$url, "catalogs/", catalogid, "/repositories/",
                 repo, "/freetext/indices/", indexName)
  }

  invisible(ag_put(service = service, url = url, queryargs = queryargs,
                   body = body))
}


modifyFreeTextIndex = function(service, catalogid = "root", repo = "",
                               indexName, predicate = NULL, indexLiterals = NULL, indexResources = FALSE,
                               indexFields = NULL, minimumWordSize = NULL, stopWords = NULL, wordFilter = NULL,
                               innerChars = NULL, borderChars = NULL, tokenizer = NULL,reIndex = NULL) {

  il = NULL
  if(!missing(indexLiterals)) il = TRUE
  queryargs = convertLogical(expandUrlArgs(list(predicate = predicate,
                                                indexLiterals = il,indexLiteralType = indexLiterals, indexResources = indexResources,
                                                indexField = indexFields, minimumWordSize = minimumWordSize, stopWord = stopWords,
                                                wordFilter = wordFilter, innerChars = innerChars, borderChars = borderChars,
                                                tokenizer = tokenizer,reIndex = reIndex)))
  body = NULL
  filepath = NULL

  if (is.list(stopWords) & length(stopWords) == 0)
    stopWord = ""
  if (is.list(indexFields) & length(indexFields) == 0)
    indexFields = ""
  if (is.list(predicate) & length(predicate) == 0)
    predicate = ""
  if (is.list(wordFilter) & length(wordFilter) == 0)
    wordFilter = ""

  if (catalogid == "root") {
    url = paste0(service$url, "repositories/", repo, "/freetext/indices/",
                 indexName)
  } else {
    url = paste0(service$url, "catalogs/", catalogid, "/repositories/",
                 repo, "/freetext/indices/", indexName)
  }

  invisible(ag_post(service = service, url = url, queryargs = queryargs,
                   body = body,filepath = NULL))
}



#' Return free-text index configuration
#'
#' @description Return information on the freetext-index named indexName.
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
#' service = createService('localhost','user','password')
#' createFreeTextIndex(service,repo = 'testRepo',index = 'index')
#' getFreeTextIndexConfiguration(service,catalogid = 'root',repo = 'testRepo',index = 'index')
#' }
getFreeTextIndexConfiguration = function(service, catalogid = "root", repo = "",
                                         index) {

  if (missing(index)) {
    cat("No index entered. These are the following free-text-indices currently present: \n \n")
    return(listFreeTextIndices(service, catalogid = catalogid, repo = repo))
  }

  queryargs = NULL
  body = NULL

  if (catalogid == "root") {
    url = paste0(service$url, "repositories/", repo, "/freetext/indices/",
                 index)
  } else {
    url = paste0(service$url, "catalogs/", catalogid, "/repositories/",
                 repo, "/freetext/indices/", index)
  }

  return(ag_get(service = service, url = url, queryargs = queryargs,
                body = body))
}




#' Query using free-text indices
#'
#' @description Query the repository using a free-text index
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param pattern A string to match triples on
#' @param index The index you want to use
#' @param limit Integer, number of triples to return
#' @param offset Integer, number of triples to skip
#' @param sorted TRUE or FALSE, defaults to FALSE
#'
#' @return A list describing all configuration parametes of index of interest
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService('localhost','user','password')
#' createFreeTextIndex(service,repo = 'testRepo',index = 'index')
#' getFreeTextIndexConfiguration(service,catalogid = 'root',repo = 'testRepo',index = 'index')
#' }
evalFreeTextSearch = function(service, catalogid = "root", repo = "", pattern,
                              index = NULL, limit = NULL, offset = NULL, sorted = NULL) {

  queryargs = convertLogical(expandUrlArgs(list(pattern = pattern, limit = limit,
                                                offset = offset, index = index, sorted = sorted)))
  body = NULL

  if (catalogid == "root") {
    url = paste0(service$url, "repositories/", repo, "/freetext")
  } else {
    url = paste0(service$url, "catalogs/", catalogid, "/repositories/",
                 repo, "/freetext")
  }

  return(ag_get(service = service, url = url, queryargs = queryargs,
                body = body))
}

#' Delete a free-text index
#'
#' @description Return a list of freetext indices defined on the store
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param indexName Name of index to delete
#'
#' @return TRUE or FALSE depending on success of deletion
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService('localhost','user','password')
#' deleteFreeTextIndex(service,catalogid = 'root',repo = 'testRepo')
#' }
deleteFreeTextIndex = function(service, catalogid = "root", repo = "",indexName) {

  queryargs = NULL
  body = NULL

  if (catalogid == "root") {
    url = paste0(service$url, "repositories/", repo, "/freetext/indices/",indexName)
  } else {
    url = paste0(service$url, "catalogs/", catalogid, "/repositories/",
                 repo, "/freetext/indices/",indexName)
  }

  invisible(ag_delete(service = service, url = url, queryargs = queryargs,
                body = body))
}




############ REGULAR INDICES


#' Return indices in repository
#'
#' @description Return a list of indices defined on the triple-store
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param valid Default FALSE. If true, then all possible index flavors are returned rather than just the ones that are currently defined.
#'
#' @return List of active indices, or all possible valid indices if 'valid' is set to TRUE
#' @export
listIndices = function(service, catalogid = "root", repo = "",valid = FALSE) {

  queryargs = convertLogical(list(listValid = valid))
  body = NULL

  if (catalogid == "root") {
    url = paste0(service$url, "repositories/", repo, "/indices")
  } else {
    url = paste0(service$url, "catalogs/", catalogid, "/repositories/indices")

  }

  return(ag_get(service = service, url = url, queryargs = queryargs,
                body = body))
}

#' Add index to repository
#'
#' @description Add the index type from the triple-store.
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param type Type of index. Use listIndices(...,valid = TRUE) to see all possible indices
#' @param style Specifies the chunk style of the new index. Can be 0, 1 or 2
#'
#' @return An index of type to the triple-store. (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService('localhost','user','password')
#' addIndex(service,repo = "testRepo",type = "gspoi",style = 0)
#' }
addIndex = function(service, catalogid = "root", repo = "", type, style = c(0,1,2)) {


  queryargs = convertLogical(list(style = style))
  body = NULL
  filepath = NULL

  if (catalogid == "root") {
    url = paste0(service$url, "repositories/", repo, "/indices/",type)
  } else {
    url = paste0(service$url, "catalogs/", catalogid, "/repositories/indices/",type)
  }

  invisible(ag_put(service = service, url = url, queryargs = queryargs,
                body = body,filepath = filepath))
}

#' Drop an index from the repository
#'
#' @description Drop the index type from the triple-store.
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param type Type of index. Use listIndices(...,valid = TRUE) to see all possible indices
#'
#' @return TRUE or FALSE stating whether or not deletion was successful (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService('localhost','user','password')
#' dropIndex(service,repo = "testRepo",type = "gspoi",style = 0)
#' }
dropIndex = function(service, catalogid = "root", repo = "", type) {


  queryargs = NULL
  body = NULL

  if (catalogid == "root") {
    url = paste0(service$url, "repositories/", repo, "/indices/",type)
  } else {
    url = paste0(service$url, "catalogs/", catalogid, "/repositories/indices/",type)
  }

  invisible(ag_delete(service = service, url = url, queryargs = queryargs,
                   body = body))
}

#' Optimize the indices
#'
#' @description Optimize the triple-store indices
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param level Specifies how aggressive the optimization will be (more aggression, more optimization, but takes more time). Has to be a positive integer (Defaults to 2)
#' @param wait Default FALSE. If TRUE, then the service does not return until the optimization is complete
#' @param index The name of the index to operate on; this can be specified multiple times in a list
#'
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService('localhost','user','password')
#' optimizeIndices(service,repo = "testRepo",index = list("gspoi","i"),level = 2,wait = TRUE)
#' }
optimizeIndices = function(service, catalogid = "root", repo = "", level = 2,wait = FALSE,index = NULL) {

  if(level <= 0) stop('level must be NULL, or a positive integer')

  queryargs = convertLogical(expandUrlArgs(list(level = level, wait = wait,index = index)))
  body = NULL
  filepath = NULL

  if (catalogid == "root") {
    url = paste0(service$url, "repositories/", repo, "/indices/optimize")
  } else {
    url = paste0(service$url, "catalogs/", catalogid, "/repositories/indices/optimize")
  }

  invisible(ag_post(service = service, url = url, queryargs = queryargs,
                   body = body,filepath = filepath))
}
