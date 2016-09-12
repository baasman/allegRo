#' List all repositories in chosen catalog
#'
#' @description Return a list of repositories available in the catalog.
#'
#' @param service Service object containing service url, username, and password
#' @param catalogid id for catalog of interest
#' @param all Default FALSE. If TRUE, then all repositories of all catalogs will be returned
#' @return A list of all repositories in the catalog. Will show all repositories if all is set to TRUE
#' @export
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' listRepositories(service)
#' }
#' @import httr
listRepositories = function(service,catalogid = "root",all = FALSE){

  queryargs = convertLogical(list(all = all))
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

listRepositories = function(service,all = FALSE){
  queryargs = convertLogical(list(all = all))
  body = NULL
  url = paste0(service$url,"repositories")
  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}




#' Create a repository
#'
#' @description Creates a new, empty repository.
#' When a repository with the given name already exists, it is overwritten, unless a parameter override with value FALSE is passed.
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param expectedSize Specifies the expected size of the repository.
#' @param index Can be specified mulitple times in a list. Should hold index names, and is used to configure the set of indices created for the store.
#' @param override Default TRUE
#' @param restore Restore the repository.
#' @param nocommit Boolean
#'
#' @return An object of ag with the response and given url.
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' createRepository(service,catalogid = "root",repo = "test",
#' expectedSize = 100,index = NULL,override = "true",restore = NULL,nocommit = 1)
#' }
#' @import httr
createRepository = function(service,catalogid = "root",repo = "",
                            expectedSize = NULL,index = NULL,override = TRUE,
                            restore = NULL,nocommit = NULL){

  queryargs = convertLogical(list(expectedSize=expectedSize,index = index,override = override,restore = restore,nocommit = nocommit))
  body = NULL
  filepath = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo)
  }

  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
}




#' Delete a Repository
#'
#' @description Delete the specified repository. This can fail if the repository is being accessed by another client.
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @return Logical statement in which TRUE implies a successful deletion (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' createRepository(service,catalogid = "root",repo = "repotodelete")
#' deleteRepository(service,catalogid = "root",repo = "repotodelete")
#' }
#' @import httr
deleteRepository = function(service, catalogid = "root",repo = ""){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo)
  }

  invisible(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}

#' Create a backup for the  repository
#'
#' @description Create a backup of the repository.
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param backupName character value, name of backup repository
#' @return AllegroGraph default reponse
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' backupRepository(service,catalogid = "root",repo = "testRepo",backupName = "backup_testRepo")
#' }
#' @import httr
backupRepository = function(service, catalogid = "root",repo = "", backupName){

  queryargs =list(target = backupName)
  body = NULL
  filepath = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/backup")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/backup")
  }

  invisible(ag_post(service = service,url = url,queryargs = queryargs,body = body))
}





#' Return a list of scripts
#'
#' @description Return the list of scripts that have been added to this repository. These scripts can either be in Lisp or Javascript code.
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @return list of scripts that have been added to this repository
#' @export
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' listScripts(service,repo = "testRepo")
#' }
#' @import httr
listScripts = function(service,catalogid = "root",repo = ""){
  queryargs = NULL
  body = NULL
  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/scripts")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/scripts")
  }
  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' Return all namespaces in repository
#'
#' @description Return the currently active namespaces for your user in this repository
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#'
#' @return An ag object that includes the list of namespaces used in the specified repository.
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' listNameSpaces(service,catalogid = "root",repo = "testRepo")
#' }
#' @import httr
listNameSpaces = function(service,catalogid= "root",repo = "test"){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/namespaces")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/namespaces")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' Delete all namespaces in repository
#'
#' @description Deletes all namespaces in this repository for the current user.
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param reset Defaults to FALSE. If TRUE,then the namespaces are reset to the default namespaces of the server.
#'
#' @return An ag delete object that states whether delete was successful or not (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' deleteAllNameSpaces(service,catalogid = "root",repo = "testRepo")
#' }
#' @import httr
deleteAllNameSpaces = function(service,catalogid= "root",repo = "",reset = FALSE){

  queryargs = convertLogical(list(reset = reset))
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/namespaces")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/namespaces")
  }

  invisible(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}

#' Delete a namespace in repository
#'
#' @description Deletes a namespace in this repository for the current user.
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param namespace Namespace id to delete
#'
#' @return An ag delete object that states whether delete was successful or not (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' deleteNameSpace(service,catalogid = "root",repo = "testRepo")
#' }
#' @import httr
deleteNameSpace = function(service,catalogid= "root",repo = "",namespace){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/namespaces/",namespace)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/namespaces/",namespace)
  }

  invisible(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}



#' Add a namespace to the repository
#'
#' @description Add a namespace for the current user in this repository
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param prefix Prefix
#' @param nsURI Namespace URI
#'
#' @return ag put object that states whether or not push was successful (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' addNameSpace(service,catalogid = "root",repo = "testRepo",prefix = "tmp",
#'              nsURI = "http://test.com/tmp#")
#' }
#' @import httr
addNameSpace = function(service,
                        catalogid = "root",
                        repo = "",
                        prefix,
                        nsURI){

  queryargs = NULL
  body = nsURI
  filepath = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/namespaces/",prefix)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/namespaces/",prefix)
  }

  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body,filepath = NULL))
}



#' Return the size of a repository
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param context Can be a list. If supplied, the count is only for the specified named graphs
#' @param session If working in a session, specify response of createSession call
#'
#' @return Number of triples in store
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' getSize(service,catalogid = "root",repo = "testRepo")
#' }
#' @import httr
getSize = function(service,catalogid= "root",repo = "",context = NULL,session = NULL){

  queryargs = expandUrlArgs(list(context = context))
  body = NULL

  if(is.null(session)){
    if(catalogid == "root"){
      url = paste0(service$url,"repositories/",repo,"/size")
    } else{
      url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/size")
    }
  } else{
    url = paste0(service$url,"session/",stringr::str_split_fixed(session$return,":",3)[,3],"/size")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}



#' Repository access
#'
#' @description Return the access permissions the current user has for this repository.
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#'
#' @return String describing access of user.
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' getAccess(service,catalogid = "root",repo = "testRepo")
#' }
#' @import httr
getAccess = function(service,catalogid= "root",repo = "testRepo"){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/access")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/access")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}



#' List all contexts in repository
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#'
#' @return list of all contexts
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' listContexs(service,catalogid = "root",repo = "testRepo")
#' }
#' @import httr
listContexts = function(service,catalogid= "root",repo = ""){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/contexts")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/contexts")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}




#' Get all duplicate statements
#'
#' @section Warning:
#' This is slow on large repositories, even if no duplicates exist.
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#'
#' @return A matrix containing all duplicate statements
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' print(getDuplicates(service,repo = "testRepo"))
#' }
getDuplicates = function(service,catalogid = "root",repo = ""){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/statements/duplicates")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repo,"/statements/duplicates")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' Delete all duplicate statements
#'
#' @description Remove duplicate triples from the repository
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param mode A string (default: spog)
#' @param commit Commit defaults to true if the connection has autocommit mode on.
#'
#' @return An ag delete object that states whether delete was successful or not (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' deleteDuplicates(service,repo = "testRepo")
#' }
deleteDuplicates = function(service,catalogid = "root",repo = "testRepo",mode = NULL,commit = NULL){

  queryargs = convertLogical(list(mode = mode,commit = commit))
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/statements/duplicates")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repo,"/statements/duplicates")
  }

  invisible(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}



#' evalQuery
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
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
#' @param convert Convert variable to R type
#'
#' @return an ag object, which includes the triples in matrix format
#' @export
#'
#' @examples
#' \dontrun{
#' query = "select ?s ?p ?o {?s ?p ?o}"
#' service = createService("localhost","user","password")
#' evalQuery(service,catalogid = "root",repo = "testRepo",
#' query = query, returnType = "data.table",
#' cleanUp = TRUE, limit = 10)
#' }
#' @import httr
#' @import data.table
evalQuery = function(service,catalogid = "root",repo = "test",query,returnType = c("data.table","dataframe","matrix","list"),
                     infer = NULL,context = NULL, cleanUp = FALSE,namedContext = NULL,callback = NULL,bindings = NULL,planner = NULL,
                     checkVariables = NULL, count = FALSE,accept = NULL,limit = 100,convert = FALSE){

  returnType = match.arg(returnType)
  if(returnType=="data.table"){
    if( !("data.table" %in% installed.packages()[,"Package"])) stop("data.table is not installed")
  }

  body = NULL

  queryargs = list(query = query,limit = limit,infer = infer, context = context, namedContext = namedContext,
                   planner = planner, checkVariables = checkVariables)

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repo)
  }

  invisible(ag_data(service = service,url = url,queryargs = queryargs,body = body,returnType = returnType,cleanUp = cleanUp,convert = convert))
}



evalStoredQueries = function(service,catalogid = "root",repo = "",queryName = "statements",bound = NULL,limit = NULL,offset = NULL,
                             cleanUp = FALSE, convert = FALSE,session = NULL){
  queryargs = list(bound = bound,limit = limit,offset = offset)
  body = NULL
  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/queries/",queryName)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/queries/",queryName)
  }
  return(ag_data(service = service,url = url,queryargs = queryargs,body = body,cleanUp = cleanUp,convert = convert))
}



