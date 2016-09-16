
#' Get statements
#'
#' @description Can specify any part of statement, and all matches will be returned.
#' If no subject, predicate, or object supplied, all statements will be returned.
#'
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param subj Subject triple pattern you want to match - optional
#' @param pred Predicate of triple pattern you want to match - optional
#' @param obj Object of triple pattern you want to match - optional
#' @param context Context of triple pattern you want to match - optional
#' @param infer Specifies the kind of inference to use for this query. Defaults to FALSE, can also be 'rdfs++' or 'hasvalue'
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
#' rep = repository(catalog(service,"root"),"test")
#' getStatements(rep,
#' pred = '<http://test.org/pred>', limit = 10)
#' }
#' @import httr
getStatements = function(repository,
                         subj = NULL, pred = NULL, obj = NULL, context = NULL, infer = FALSE,
                         limit = NULL, tripleIDs = FALSE, count = FALSE, returnType = c("data.table",
                                                                                        "dataframe", "matrix", "list"), cleanUp = FALSE, convert = FALSE) {

  if (missing(returnType)) {
    returnType = "matrix"
  } else {
    returnType = match.arg(returnType)
  }

  body = NULL
  filepath = NULL

  if (missing(subj) & missing(pred) & missing(obj) & missing(context)) {
    queryargs = NULL
    url = paste0(repository$url, "statements")
    return(ag_get(service = repository, url = url, queryargs = queryargs,
                  body = body))
  }

  subjEnd = predEnd = objEnd = NULL
  # if(!missing(subj)) subjEnd = subj
  #if(!missing(pred)) predEnd = pred
  # if(!missing(obj)) objEnd = obj


  queryargs = convertLogical(expandUrlArgs(list(subj = subj, subjEnd = subjEnd,
                                                pred = pred, predEnd = predEnd, obj = obj, objEnd = objEnd, context = context,
                                                infer = infer, limit = limit, tripleIDs = tripleIDs, count = count)))

  url = paste0(repository$url, "statements")


  return(ag_statements(service = repository, url = url, queryargs = queryargs,
                          body = body, returnType = returnType, cleanUp = cleanUp, convert = convert))
}


#' Add a statement
#'
#' @description Add a triple to the repository.
#'The subj, pred, and obj arguments must be supplied and are used to fill in the subject, predicate and object parts of the triple. The context argument is used to fill in the triple's graph but may be left off. In this case, the triple's graph will be the default graph of the repository.
#'
#'The subj, pred, obj, and context (if supplied) arguments must all be in URI encoded N-Triples notation.
#'
#'The service returns the triple-ID of the newly added triple.
#'
#' @param repository Object of type repository specifying server details and repository to work on.
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
#' rep = repository(catalog(service,"root"),"test")
#' subj = '<www.test.com/tmp#person>'
#' pred = '<www.test.com/tmp#hasItem>'
#' obj= '<www.test.com/tmp#sword>'
#' addStatement(rep,
#' subj = subj,pred = pred,obj = obj)
#' }
#' @import httr
addStatement = function(repository,
                        subj = NULL, pred = NULL, obj = NULL, context = NULL) {

  queryargs = list(subj = subj, pred = pred, obj = obj, context = context)

  body = NULL
  filepath = NULL

  url = paste0(repository$url, "statement")

  invisible(ag_put(service = repository, url = url, queryargs = queryargs,
                   body = body, filepath = filepath))
}

#' Add a list of triples
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param quads A list of character vectors, where each vector representes a triples and has 4 elements.
#' The graph component can be NULL or ""
#'
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' triples = list(c("<a>","<b>","<c>","<g1>"),
#'                c("<a>","<b>","<c>","<g2>"))
#' addStatements(rep,triples)
#' }
addStatements = function(repository,
                        quads) {
  queryargs = NULL
  body = ifelse(class(quads) == "json",quads,jsonlite::toJSON(quads))
  filepath = NULL
  url = paste0(repository$url, "statements")
  invisible(ag_post(service = repository, url = url, queryargs = queryargs,
                   body = body, filepath = filepath,type = TRUE))
}


#' Delete matching statements
#'
#' @description Delete statements (triples) by matching against their components.
#'All parameters are optional -- when none are given, every statement in the store is deleted.
#'The patterns to match come in pairs for subject, predicate, object, and graph. When given, they should be specified in N-triples notation.
#'
#' @param repository Object of type repository specifying server details and repository to work on.
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
#' rep = repository(catalog(service,"root"),"test")
#' subj = '<www.test.com/tmp#person>'
#' pred = '<www.test.com/tmp#hasItem>'
#' obj= '<www.test.com/tmp#sword>'
#' deleteStatements(rep,
#' subj = subj,pred = pred,obj = obj)
#' }
#' @import httr
deleteStatements = function(repository,
                            subj = NULL, pred = NULL, obj = NULL, context = NULL) {

  if (missing(subj) & missing(pred) & missing(obj) & missing(context)) {
    if (readline("Since no patterns were specified, all statements will be deleted. Is this what you wanted? Type yes or no: ") ==
        "yes") {
      body = NULL
      queryargs = NULL
      url = paste0(repository$url, "statements")
      invisible(ag_delete(service = repository, url = url, queryargs = queryargs,
                          body = body))
    } else {
      stop("To delete matching statements, include either the subject, predicate, object, or graph")
    }
  }

  subjEnd = predEnd = objEnd = NULL
  # if (!missing(subj))
  #   subjEnd = subj
  # if (!missing(pred))
  #   predEnd = pred
  # if (!missing(obj))
  #   objEnd = obj


  queryargs = list(subj = subj, subjEnd = subjEnd, pred = pred, predEnd = predEnd,
                   obj = obj, objEnd = objEnd, context = context)

  body = NULL
  url = paste0(repository$url, "statements")

  invisible(ag_delete(service = repository, url = url, queryargs = queryargs,
                      body = body))
}




#' Add statements from a file
#'
#' @description To be added
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param filepath File that contains your triples
#' @param baseURI ...
#' @param context Context to attach to added triples
#' @param commitEvery ...
#'
#' @return an ag object that says whether or not the push was successful.
#' @export
#'
#' @examples
#'\dontrun{
#' service = createService("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' addStatementsFromFile(rep,
#' file = 'path/to/file/mytriples.nq')
#' }
#' @import httr
addStatementsFromFile = function(repository,
                                 filepath, baseURI = NULL, context = NULL, commitEvery = NULL) {

  if (missing(filepath))
    stop("must supply path of file to be uploaded")


  queryargs = list(context = context, baseURI = baseURI, commit = commitEvery)

  body = checkFormat(filepath)

  url = paste0(repository$url, "statements")

  invisible(ag_put(service = repository, url = url, queryargs = queryargs,
                   body = body, filepath = filepath))
}




#' Get all duplicate statements
#'
#' @section Warning:
#' This is slow on large repositories, even if no duplicates exist.
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#'
#' @return A matrix containing all duplicate statements
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' getDuplicates(rep)
#' }
getDuplicates = function(repository){

  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"statements/duplicates")

  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Delete all duplicate statements
#'
#' @description Remove duplicate triples from the repository
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param mode A string (default: spog)
#' @param commit Commit defaults to true if the connection has autocommit mode on.
#'
#' @return An ag delete object that states whether delete was successful or not (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' deleteDuplicates(rep)
#' }
deleteDuplicates = function(repository,mode = NULL,commit = NULL){

  queryargs = convertLogical(list(mode = mode,commit = commit))
  body = NULL

  url = paste0(repository$url,"statements/duplicates")

  invisible(ag_delete(service = repository,url = url,queryargs = queryargs,body = body))
}
















### testing bulk loader

#
# agLoad = function(service, catalogid = "root", repo = "", url = NULL, filepath,
#                   bulkLoad = NULL, bulkLoaders = NULL, externalReferences = NULL, baseURI = NULL,
#                   context = NULL) {
#
#   if (missing(filepath))
#     stop("must supply path of file to be uploaded")
#
#   queryargs = list(url = url, filepath = filepath, bulkLoad = bulkLoad,
#                    bulkLoaders = bulkLoaders, externalReferences = externalReferences,
#                    baseURI = baseURI, context = context, useAgload = "1")
#
#   if (grepl("nq", filepath) | grepl("nt", filepath)) {
#     body = quote(upload_file(path = filepath, type = "text/plain"))
#   } else if (grepl("rdf", filepath) | grepl("xml", filepath)) {
#     body = quote(upload_file(path = filepath, type = "application/rdf+xml"))
#   }
#
#   if (catalogid == "root") {
#     url = paste0(service$url, "repositories/", repo, "/statements")
#   } else {
#     url = paste0(service$url, "catalogs/", catalogid, "/repositories/",
#                  repo, "/statements")
#   }
#
#   invisible(ag_put(service = service, url = url, queryargs = queryargs,
#                    body = body, filepath = filepath))
# }
