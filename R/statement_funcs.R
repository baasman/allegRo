#' getStatements
#'
#' @description Can specify any part of statement, and all matches will be returned.
#' If no subject, predicate, or object supplied, all statements will be returned.
#'
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repositoryid Id for repository of interest.
#' @param subj Subject triple pattern you want to match - optional
#' @param pred Predicate of triple pattern you want to match - optional
#' @param obj Object of triple pattern you want to match - optional
#' @param context Context of triple pattern you want to match - optional
#' @param infer ...
#' @param limit How many triples should be returned
#' @param tripleIDs ...
#' @param count ...
#' @param returnType What type the return value should be. Best performance achieved with matrix.
#' @param cleanUp Should the xml schemas be removed. Has to be true for converting to R types to work
#' @param convert Convert variable to appropriate R type
#'
#' @return Triples based on the specified pattern
#' @export
#'
#' @examples
#'\dontrun{
#' service = createService("localhost","user","password")
#' getStatements(service,catalogid = "root",repositoryid = "testRepo",
#' pred = "<http://test.org/pred>", limit = 10)
#' }
#' @import httr
getStatements = function(service,catalogid = "root",repositoryid = "testRepo", subj = NULL,
                         pred = NULL,obj = NULL, context = NULL,infer = "false",
                         limit = NULL, tripleIDs = "false",count = "false",
                         returnType = c("data.table","dataframe","matrix","list"),
                         cleanUp = FALSE,convert = FALSE){

  if(missing(subj) & missing(pred) & missing(obj)){
    body = NULL
    queryargs = NULL
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

  subjEnd = predEnd = objEnd = NULL
  if(!missing(subj)) subjEnd = subj
  if(!missing(pred)) predEnd = pred
  if(!missing(obj)) objEnd = obj


  queryargs = list(subj = subj, subjEnd = subjEnd, pred = pred, predEnd = predEnd, obj = obj, objEnd = objEnd, context = context,infer = infer,
                   limit = limit,tripleIDs = tripleIDs,count = count)

  body = NULL
  filepath = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/statements")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repositoryid,"/statements")
  }

  invisible(ag_statements(service = service,url = url,queryargs = queryargs,body = body,returnType = returnType,cleanUp = cleanUp,convert = convert))
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
addStatement = function(service,catalogid = "root",repositoryid = "testRepo", subj = "<s>",
                        pred = "<o>",obj = "<p>", context = NULL){

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



#' deleteStatements
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
#' deleteStatements(service,catalogid = "root",reposirepositoryid = "testRepo",
#' subj = subj,pred = pred,obj = obj)
#' }
#' @import httr
deleteStatements = function(service,catalogid = "root",repositoryid = "testRepo", subj = NULL,
                        pred = NULL,obj = NULL, context = NULL){

  if(missing(subj) & missing(pred) & missing(obj)){
    if(
      readline("Since no patterns were specified, all statements will be deleted. Is this what you wanted? Type yes or no: ") == "yes"){
      body = NULL
      queryargs = NULL
      if(catalogid == "root"){
        url = paste0(service$url,"repositories/",repositoryid,"/statements")
      } else{
        url = paste0(service$url,"catalogs/",catalogid,
                     "/repositories/",repositoryid,"/statements")
      }
      invisible(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
    } else{
      stop("To delete matching statements, include either the subject, predicate, object, or graph")
    }
  }

  subjEnd = predEnd = objEnd = NULL
  if(!missing(subj)) subjEnd = subj
  if(!missing(pred)) predEnd = pred
  if(!missing(obj)) objEnd = obj


  queryargs = list(subj = subj, subjEnd = subjEnd, pred = pred, predEnd = predEnd, obj = obj, objEnd = objEnd,context = context)

  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/statements")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repositoryid,"/statements")
  }

  invisible(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
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
