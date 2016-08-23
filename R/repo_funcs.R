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

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
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

  body = NULL
  filepath = NULL

  queryargs = list(expectedSize=expectedSize,index = index,override = override,restore = restore,nocommit = nocommit)
  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid)
  }

  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
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
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid)
  }

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
listNameSpaces = function(service,catalogid= "root",repositoryid = "test"){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/namespaces")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid,"/namespaces")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' deleteNameSpaces
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#' @param reset Boolean, If 1, then the namespaces are reset to the default namespaces of the server.
#'
#' @return An ag delete object that states whether delete was successful or not
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' deleteNameSpaces(service,catalogid = "root",repositoryid = "testRepo")
#' }
#' @import httr
deleteNameSpaces = function(service,catalogid= "root",repositoryid = "testWithParsa",reset = c(0,1,"0","1")){

  if(!missing(reset)) reset = match.arg(reset)
  if(is.numeric(reset)) reset = as.character(reset)

  queryargs = list(reset = reset)

  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/namespaces")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid,"/namespaces")
  }

  return(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}



#' addNameSpace
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#' @param prefix Prefix
#' @param nsURI Namespace URI
#'
#' @return ag put object
#' @export
#'
#' \dontrun{
#' service = createService("localhost","user","password")
#' addNameSpace(service,catalogid = "root",repositoryid = "testRepo",prefix = "tmp",
#'              nsURI = "<http://test.com/tmp#>)
#' }
#' @import httr
addNameSpace = function(service,catalogid = "root",repositoryid = "newTest",prefix, nsURI){

  queryargs = NULL
  body = nsURI
  filepath = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/namespaces/",prefix)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid,"/namespaces/",prefix)
  }

  return(ag_put(service = service,url = url,queryargs = queryargs,body = body,filepath = NULL))
}



#' getSize
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#'
#' @return An ag get object that shows how many triples are in the repository
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' getSize(service,catalogid = "root",repositoryid = "testRepo")
#' }
#' @import httr
getSize = function(service,catalogid= "root",repositoryid = "test"){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/size")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid,"/size")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' addStatement
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#' @param subj valid url
#' @param pred valid url
#' @param obj valid url
#' @param context context of triple
#'
#' @return Return: successful push or not
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' subj = "<www.test.com/tmp#person>"
#' pred = "<www.test.com/tmp#hasItem>"
#' obj= "<www.test.com/tmp#sword>"
#' }
#' @import httr
getStatements = function(service,catalogid = "root",repositoryid = "testRepo", subj = "s",
                        pred = "o",obj = "p", context = NULL,infer = "false",callback = NULL,
                        limit = NULL, tripleIDs = "false",count = "false"){

  if(missing(subj) | missing(pred) | missing(obj)) stop("subj,pred, and obj must all be included in call")
  if(missing(subj) & missing(pred) & missing(obj)){
    if(catalogid == "root"){
      url = paste0(service$url,"repositories/",repositoryid,"/statements")
    } else{
      url = paste0(service$url,"catalogs/",catalogid,
                   "/repositories/",repositoryid,"/statements")
    }
    body = NULL
    queryargs = NULL
    return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
  }

  queryargs = list(subj = subj, pred = pred, obj = obj,context = context,infer = infer,callback = callback,
                   limit = limit,tripleIDs = tripleIDs,count = count)

  body = NULL
  filepath = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/statements/query")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repositoryid,"/statements/query")
  }

  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
}



#' addStatement
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#' @param subj valid url
#' @param pred valid url
#' @param obj valid url
#' @param context context of triple
#'
#' @return Return: successful push or not
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' subj = "<www.test.com/tmp#person>"
#' pred = "<www.test.com/tmp#hasItem>"
#' obj= "<www.test.com/tmp#sword>"
#' addStatement(service,catalogid = "root",reposirepositoryid = "testRepo",
#' subj = subj,pred = pred,obj = obj)
#' }
#' @import httr
addStatement = function(service,catalogid = "root",repositoryid = "testRepo", subj = "s",
                        pred = "o",obj = "p", context = NULL){

  queryargs = list(subj = subj, pred = pred, obj = obj,context = context)
  body = NULL
  filepath = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/statement")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repositoryid,"/statement")
  }

  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
}






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
addStatementsFromFile = function(service,catalogid = "root",repositoryid = "",
                                 filepath,baseURI = NULL,context=NULL,commitEvery = NULL){

  if(missing(filepath)) stop("must supply path of file to be uploaded")

  queryargs = list(context = context,baseURI = baseURI,commit = commitEvery)
  body = quote(upload_file(path = filepath,type = "text/plain"))

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/statements")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repositoryid,"/statements")
  }

  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
}




#' evalQuery
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#' @param query Sparql query to evaluate
#' @param returnType In what format should the triples be returned. Can choose between "dataframe","matrix","list". Defaults to "list
#' @param cleanUp If TRUE, removes the XML types of all variables. Currently for this to work, the return type needs to be a data.table. However, this is recommended either way.
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
#' evalQuery(service,catalogid = "root",repositoryid = "testRepo", query = query, returnType = "data.table",
#' cleanUp = TRUE, limit = 10)
#' }
#' @import httr
#' @import data.table
#' @importFrom utils, installed.packages
evalQuery = function(service,catalogid = "root",repositoryid = "test",query,returnType = c("data.table","dataframe","matrix","list"),infer = NULL,context = NULL,
                     cleanUp = FALSE,namedContext = NULL,callback = NULL,bindings = NULL,planner = NULL,checkVariables = NULL,
                     count = FALSE,accept = NULL,limit = 100){

  returnType = match.arg(returnType)
  if(returnType=="data.table"){
    if( !("data.table" %in% installed.packages()[,"Package"])) stop("data.table is not installed")
  }

  body = NULL

  queryargs = list(query = query,limit = limit,infer = infer, context = context, namedContext = namedContext,
                   planner = planner, checkVariables = checkVariables)

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repositoryid)
  }

  invisible(ag_data(service = service,url = url,queryargs = queryargs,body = body,returnType = returnType,cleanUp = cleanUp))
}
