#' List all repositories in chosen catalog
#'
#' @description Return a list of repositories available in the catalog.
#'
#' @param catalog Catalog object specifying username, password, and catalog to work on.
#' @param all Default FALSE. If TRUE, then all repositories of all catalogs will be returned
#' @return A list of all repositories in the catalog. Will show all repositories if all is set to TRUE
#' @export
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' cat = catalog(service,"root")
#' listRepositories(cat)
#' }
#' @import httr
listRepositories = function(catalog,all = FALSE){
  queryargs = convertLogical(list(all = all))
  body = NULL
  url = paste0(catalog$url,"repositories")
  return(ag_get(service = catalog,url = url,queryargs = queryargs,body = body))
}


#' Create a repository
#'
#' @description Creates a new, empty repository.
#' When a repository with the given name already exists, it is overwritten, unless a parameter override with value FALSE is passed.
#'
#' @param catalog Catalog object specifying username, password, and catalog to work on.
#' @param repo Id for repository you want to create.
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
#' service = service("localhost","user","password")
#' cat = catalog(service,"root")
#' createRepository(cat,repo = "test",
#' expectedSize = 100,index = NULL,override = "true",restore = NULL,nocommit = 1)
#' }
#' @import httr
createRepository = function(catalog, repo,
                            expectedSize = NULL,index = NULL,override = FALSE,
                            restore = NULL,nocommit = NULL){

  queryargs = convertLogical(list(expectedSize=expectedSize,index = index,override = override,restore = restore,nocommit = nocommit))
  body = NULL
  filepath = NULL
  url = paste0(catalog$url,"repositories/",repo)

  invisible(ag_put(service = catalog,url = url,queryargs = queryargs,body = body,filepath = filepath))
}




#' Delete a Repository
#'
#' @description Delete the specified repository. This can fail if the repository is being accessed by another client.
#'
#' @param catalog Catalog object specifying username, password, and catalog to work on.
#' @param repo Id for repository you want to create.
#' @return Logical statement in which TRUE implies a successful deletion (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' cat = catalog(service,"root")
#' createRepository(cat,repo = "repotodelete")
#' deleteRepository(cat,repo = "repotodelete")
#' }
#' @import httr
deleteRepository = function(catalog,repo){

  queryargs = NULL
  body = NULL
  url = paste0(catalog$url,"repositories/",repo)

  invisible(ag_delete(service = catalog,url = url,queryargs = queryargs,body = body))
}

#' Create a backup for the  repository
#'
#' @description Create a backup of the repository.
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param backupName character value, name of backup repository
#' @return AllegroGraph default reponse
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' backupRepository(,backupName = "backup_testRepo")
#' }
#' @import httr
backupRepository = function(repository, backupName){

  queryargs =list(target = backupName)
  body = NULL
  filepath = NULL
  url = paste0(repository$url,"/backup")

  invisible(ag_post(service = repository,url = url,queryargs = queryargs,body = body))
}




#' Return a list of scripts
#'
#' @description Return the list of scripts that have been added to this repository. These scripts can either be in Lisp or Javascript code.
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @return list of scripts that have been added to this repository
#' @export
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' listScripts(rep)
#' }
#' @import httr
listScripts = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"/scripts")
  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Return all namespaces in repository
#'
#' @description Return the currently active namespaces for your user in this repository
#' @param repository Object of type repository specifying server details and repository to work on.
#' @return An ag object that includes the list of namespaces used in the specified repository.
#' @export
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' listNameSpaces(rep)
#' }
#' @import httr
listNameSpaces = function(repository){
  queryargs = NULL
  url = paste0(repository$url,"namespaces")
  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Delete all namespaces in repository
#'
#' @description Deletes all namespaces in this repository for the current user.
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param reset Defaults to FALSE. If TRUE,then the namespaces are reset to the default namespaces of the server.
#'
#' @return An ag delete object that states whether delete was successful or not (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' deleteAllNameSpaces(rep,reset = TRUE)
#' }
#' @import httr
deleteAllNameSpaces = function(repository,reset = FALSE){

  queryargs = convertLogical(list(reset = reset))
  body = NULL
  url = paste0(repository$url,"namespaces")

  invisible(ag_delete(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Delete a namespace in repository
#'
#' @description Deletes a namespace in this repository for the current user.
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param namespace Namespace id to delete
#'
#' @return An ag delete object that states whether delete was successful or not (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' deleteNameSpace(service,catalogid = "root",repo = "testRepo",namespace = "foaf")
#' }
#' @import httr
deleteNameSpace = function(repository,namespace){

  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"namespaces/",namespace)

  invisible(ag_delete(service = repository,url = url,queryargs = queryargs,body = body))
}



#' Add a namespace to the repository
#'
#' @description Add a namespace for the current user in this repository
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param prefix Prefix
#' @param nsURI Namespace URI
#'
#' @return ag put object that states whether or not push was successful (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' addNameSpace(service,catalogid = "root",repo = "testRepo",prefix = "foaf",
#' nsURI = "http://xmlns.com/foaf/0.1/")
#' }
#' @import httr
addNameSpace = function(repository,
                        prefix,
                        nsURI){

  queryargs = NULL
  body = nsURI
  filepath = NULL
  url = paste0(repository$url,"namespaces/",prefix)

  invisible(ag_put(service = repository,url = url,queryargs = queryargs,body = body,filepath = NULL))
}



#' Return the size of a repository
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param context Can be a list. If supplied, the count is only for the specified named graphs
#'
#' @return Number of triples in store
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' getSize(rep)
#' }
#' @import httr
getSize = function(repository,context = NULL){

  queryargs = expandUrlArgs(list(context = context))
  body = NULL
  url = paste0(repository$url,"size")

  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}



#' Repository access
#'
#' @description Return the access permissions the current user has for this repository.
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#'
#' @return String describing access of user.
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' getAccess(rep)
#' }
#' @import httr
getAccess = function(repository){

  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"access")

  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}



#' List all contexts in repository
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#'
#' @return list of all contexts
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' listContexs(rep)
#' }
#' @import httr
listContexts = function(repository){

  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"contexts")

  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}



#' evalQuery
#'
#' @description Execute a SPARQL select query
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param query Sparql query to evaluate
#' @param returnType In what format should the triples be returned. Can choose between "dataframe","matrix","list". Defaults to "list
#' @param cleanUp If TRUE, removes the XML types of all variables. Currently for this to work, the return type needs to be a data.table. However, this is recommended either way.
#' @param convert Convert variable to R type
#' @param infer ...
#' @param context Can be a list. Specifies the contexts from which you want to query
#' @param namedContext ...
#' @param bindings ...
#' @param planner ...
#' @param checkVariables ...
#' @param limit How many triples to return
#'
#' @return Triples in matrix form
#' @export
#'
#' @examples
#' \dontrun{
#' query = "select ?s ?p ?o {?s ?p ?o}"
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"root")
#' evalQuery(rep,
#' query = query, returnType = "data.table",
#' cleanUp = TRUE, limit = 10)
#' }
#' @import httr
#' @import data.table
#' @import utils
evalQuery = function(repository,query,returnType = c("data.table","dataframe","matrix","list"),
                     infer = NULL,context = NULL, cleanUp = FALSE,namedContext = NULL,bindings = NULL,planner = NULL,
                     checkVariables = NULL,limit = NULL,convert = FALSE){

  returnType = match.arg(returnType)
  if(returnType=="data.table"){
    if( !("data.table" %in% installed.packages()[,"Package"])) stop("data.table is not installed")
  }

  body = NULL

  queryargs = convertLogical(expandUrlArgs(list(query = query,limit = limit,infer = infer, context = context, namedContext = namedContext,
                   planner = planner, checkVariables = checkVariables)))

  url = repository$url

  return(ag_data(service = repository,url = url,queryargs = queryargs,body = body,returnType = returnType,cleanUp = cleanUp,convert = convert))
}


#does not work yet
# evalStoredQueries = function(service,catalogid = "root",repo = "",queryName = "statements",bound = NULL,limit = NULL,offset = NULL,
#                              cleanUp = FALSE, convert = FALSE,session = NULL){
#   queryargs = list(bound = bound,limit = limit,offset = offset)
#   body = NULL
#   if(catalogid == "root"){
#     url = paste0(service$url,"repositories/",repo,"/queries/",queryName)
#   } else{
#     url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/queries/",queryName)
#   }
#   return(ag_data(service = service,url = url,queryargs = queryargs,body = body,cleanUp = cleanUp,convert = convert))
# }



