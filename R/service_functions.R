

#' List all catalogs
#' @param service Service object containing service url, username, and password
#' @return ag object
#' @export
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' listCatalogs(service)
#' }
#' @import httr
listCatalogs = function(service){
  queryargs = NULL
  url = paste0(service$url,"catalogs")
  body = NULL
  return(ag_get(service = service,url = url,queryargs,body))
}


#' List all repositories in chosen catalog
#' @param service Service object containing service url, username, and password
#' @param catalogid id for catalog of interest
#' @return ag object
#' @export
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' listCatalogs(service)
#' }
#' @import httr
listRepositories = function(service,catalogid = "root"){
  if(catalogid == "root"){
    url = paste0(service$url,"repositories")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories")
  }
  return(ag_get(service,url,NULL))
}


#' createRepository
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#' @param expectedSize Specifies the expected size of the repository.
#' @param index Can be specified mulitple times. Should hold index names, and is used to configure the set of indices created for the store.
#' @param override Override repository, 1 or 0.
#' @param restore Restore the repository.
#' @param nocommit ...
#'
#' @return An object of ag with the response and given url.
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' createRepository(service,catalogid = "root",repositoryid = "test",
#' expectedSize = 100,index = NULL,override = "true",restore = NULL,nocommit = 1)
#' }
#' @import httr
createRepository = function(service,catalogid = "root",repositoryid = "testFromR2",
                             expectedSize = NULL,index = NULL,override = c("true","false","if-not-open"),
                             restore = NULL,nocommit = c("1","0")){

  if(missing(override)) override = NULL
  if(missing(nocommit)) nocommit = NULL

  queryargs = list(expectedSize=expectedSize,index = index,override = override,restore = restore,nocommit = nocommit)
  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid)
  }
  body = NULL

  return(ag_put(service,url,queryargs,body))
}




#' Delete a Repository
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#' @return True or False, denoting whether delete was successful.
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' deleteRepository(service,catalogid = "root",repositoryid = "repotodelete")
#' }
#' @import httr
deleteRepository = function(service, catalogid = "root",repositoryid){

  queryargs = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid)
  }
  body = NULL

  invisible(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}



#' listNameSpaces
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#'
#' @return An ag object that includes the list of namespaces used in the specified repository.
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' listNameSpaces(service,catalogid = "root",repositoryid = "testRepo")
#' }
#' @import httr
listNameSpaces = function(service,catalogid= "root",repositoryid = "testWithParsa"){

  queryargs = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/namespaces")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid,"/namespaces")
  }

  body = NULL

  return(ag_get(service,url,queryargs,body))
}


#file = "C:/Users/baasman/Documents/testtrips.nq"

#' addStatementsFromFile
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#' @param filepath File that contains your triples
#' @param baseURI ...
#' @param context ...
#' @param commitEvery ...
#'
#' @return an ag object that says whether or not the push was successful.
#' @export
#'
#' @examples
#'\dontrun{
#' service = createService("localhost","user","password")
#' addStatementsFromFile(service,catalogid = "root", repositoryid = "testRepo",
#' file = "path/to/file/mytriples.nq")
#' }
#' @import httr
addStatementsFromFile = function(service,catalogid = "root",repositoryid = "testWithParsa2",
                                  filepath,baseURI = NULL,context=NULL,commitEvery = NULL){

  if(missing(filepath)) stop("must supply path of file to be uploaded")

  queryargs = list(context = context,baseURI = baseURI,commit = commitEvery)

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/statements")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repositoryid,"/statements")
  }

  body = quote(upload_file(path = filepath,type = "text/plain"))

  invisible(ag_put(service,url,queryargs,body))
}




#' evalQuery
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#' @param query Sparql query to evaluate
#' @param infer ...
#' @param context ...
#' @param namedContext ...
#' @param callback ...
#' @param bindings ...
#' @param planner ...
#' @param checkVariables ...
#' @param count ...
#' @param accept ...
#' @param limit ...
#'
#' @return an ag object, which includes the triples in matrix format
#' @export
#'
#' @examples
#' \dontrun{
#' query = "select ?s ?p ?o {?s ?p ?o}"
#' service = createService("localhost","user","password")
#' evalQuery(service,catalogid = "root",repositoryid = "testRepo", query = query, limit = 10)
#' }
#' @import httr
evalQuery = function(service,catalogid = "root",repositoryid = "testfromr5",query,returnType = c("matrix","dataframe","list"),infer = NULL,context = NULL,
                     namedContext = NULL,callback = NULL,bindings = NULL,planner = NULL,checkVariables = NULL,
                     count = FALSE,accept = NULL,limit = 100){

  returnType = match.arg(returnType)

  queryargs = list(query = query,limit = limit,infer = infer, context = context, namedContext = namedContext,
                     planner = planner, checkVariables = checkVariables)

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repositoryid)
  }

  body = NULL

  invisible(ag_data(service = service,url = url,queryargs,body,returnType))
}






