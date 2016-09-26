#' List all mappings in repository
#' @description Return the datatype and predicate mappings for the repository.
#' @param repository Object of type repository specifying server details and repository to work on.
#' @return A matrix containing the datatype and predicate mappings for the repository.
#' @export
listMappings = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"mapping")
  return(ag_get(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Delete user-defined mappings in repository
#' @description Remove any user-defined datatype and predicate mappings.
#' @param repository Object of type repository specifying server details and repository to work on.
#' @export
deleteMappings = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"mapping")
  invisible(ag_delete(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Delete all mappings in repository
#' @description Remove all datatype and predicate mappings (included the default ones).
#' @param repository Object of type repository specifying server details and repository to work on.
#' @export
deleteAllMappings = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"mapping/all")
  invisible(ag_delete(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Add new predicate mapping
#' @description Add a new predicate mapping between predicate and encoding.
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param predicate String
#' @param encoding String
#' @export
addPredicateMapping = function(repository,predicate,encoding){
  queryargs = list(predicate = predicate,encoding = encoding)
  body = NULL
  url = paste0(repository$url,"mapping/predicate")
  invisible(ag_post(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Add new mapped type
#' @description Add a new datatype mapping between type and encoding.
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param type String
#' @param encoding String
#' @export
addMappedType = function(repository,type,encoding){
  queryargs = list(type = type,encoding = encoding)
  body = NULL
  url = paste0(repository$url,"mapping/type")
  invisible(ag_post(service = repository,url = url,queryargs = queryargs,body = body))
}

#' Delete a mapped type
#' @description Remove the datatype mapping between type and encoding.
#' @param repository Object of type repository specifying server details and repository to work on.
#' @param type String
#' @export
deleteMappedType = function(repository,type){
  queryargs = list(type = type)
  body = NULL
  url = paste0(repository$url,"mapping/type")
  invisible(ag_delete(service = repository,url = url,queryargs = queryargs,body = body))
}






# service = service("http://localhost:10059",user = "test",password = "xyzzy",TRUE)
# cat = catalog(service,"root")
# createRepository(cat,repo = "testRepo",override = TRUE)
# rep = repository(cat,"testRepo")
# addMappedType(rep,"<http:time>","<http://www.w3.org/2001/XMLSchema#dateTime>")
# addStatement(rep, '"x"', "<http:happened>", "\"2009-09-28T17:41:39\"^^<http:time>")
# addStatement(rep, '"y"', "<http:happened>", "\"2009-09-28T18:22:00\"^^<http:time>")
# addStatement(rep, '"z"', "<http:happened>", "\"2009-09-28T17:02:41\"^^<http:time>")
# str(getStatements(rep,obj= list("\"2009-09-28T17:00:00\"^^<http:time>", "\"2009-09-28T19:00:00\"^^<http:time>") )$return)
# getStatements(rep,pred = "<happened>",cleanUp = TRUE)$return



