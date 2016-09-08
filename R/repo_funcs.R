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
#' @param repo Id for repository of interest.
#' @param expectedSize Specifies the expected size of the repository.
#' @param index Can be specified mulitple times in a list. Should hold index names, and is used to configure the set of indices created for the store.
#' @param override Override repository, 1 or 0.
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
                            expectedSize = NULL,index = NULL,override = FALSE,
                            restore = NULL,nocommit = NULL){

  body = NULL
  filepath = NULL

  queryargs = convertLogical(list(expectedSize=expectedSize,index = index,override = override,restore = restore,nocommit = nocommit))
  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo)
  }

  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
}




#' Delete a Repository
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @return True or False, denoting whether delete was successful.
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' deleteRepository(service,catalogid = "root",repo = "repotodelete")
#' }
#' @import httr
deleteRepository = function(service, catalogid = "root",repo){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo)
  }

  invisible(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}

#' getInitFile
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @return ag get object containing init file
#' @export
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' getInitFile(service)
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

#' listNameSpaces
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

#' deleteNameSpaces
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param reset Boolean, If 1, then the namespaces are reset to the default namespaces of the server.
#'
#' @return An ag delete object that states whether delete was successful or not
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' deleteNameSpaces(service,catalogid = "root",repo = "testRepo")
#' }
#' @import httr
deleteNameSpaces = function(service,catalogid= "root",repo = "testRepo",reset = c(0,1,"0","1")){

  if(!missing(reset)) reset = match.arg(reset)
  if(is.numeric(reset)) reset = as.character(reset)

  queryargs = list(reset = reset)

  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/namespaces")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/namespaces")
  }

  return(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}



#' addNameSpace
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param prefix Prefix
#' @param nsURI Namespace URI
#'
#' @return ag put object that states whether or not push was successful
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
                        repo = "newTest",
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



#' getSize
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param session If working in a session, specify response of createSession call
#'
#' @return An ag get object that shows how many triples are in the repository
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' getSize(service,catalogid = "root",repo = "testRepo")
#' }
#' @import httr
getSize = function(service,catalogid= "root",repo = "",session = NULL){

  queryargs = NULL
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



#' getAccess
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#'
#' @return An ag get object shows the kind of access the user has on this repository
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



#' listContexts
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#'
#' @return An ag get objects that displays all context id's
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' listContexs(service,catalogid = "root",repo = "testRepo")
#' }
#' @import httr
listContexts = function(service,catalogid= "root",repo = "testRepo"){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/contexts")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/contexts")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}




#' getDuplicates
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
getDuplicates = function(service,catalogid = "root",repo = "testRepo"){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/statements/duplicates")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repo,"/statements/duplicates")
  }

  invisible(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' deleteDuplicates
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param mode A string (default: spog)
#' @param commit A string
#'
#' @return An ag delete object
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' deleteDuplicates(service,repo = "testRepo")
#' }
deleteDuplicates = function(service,catalogid = "root",repo = "testRepo",mode = NULL,commit = NULL){

  queryargs = list(mode = mode,commit = commit)
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


#does not work yet
# evalStoredQueries = function(service,catalogid = "root",repo = "",queryName = "",bound = NULL,limit = NULL,offset = NULL,
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



