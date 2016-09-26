
#' user
#'
#' @name user
#'
#' @description Collection of functions to operate on different users on the server
#'
#' @param service The service object specifying port,username, and password
#' @param name Username
#' @param password Password associated with username
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' listUsers(service)
#' }
NULL


#' @rdname user
#' @export
listUsers = function(service){
  queryargs = NULL
  body = NULL
  url = paste0(service$url,"users")
  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' @rdname user
#' @export
addUser = function(service,name,password){
  queryargs = list(password = password)
  body = NULL
  url = paste0(service$url,"users/",name)
  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body))
}

#' @rdname user
#' @export
deleteUser = function(service,name){
  queryargs = NULL
  body = NULL
  url = paste0(service$url,"users/",name)
  invisible(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}

#' @rdname user
#' @export
changeUserPassword = function(service,name, password){
  queryargs = NULL
  body = password
  filepath = NULL
  invisible(ag_post(service = service, url = paste0(service$url,"users/",name,"/password"),queryargs = queryargs,body = body,filepath = filepath))
}

#' @rdname user
#' @export
listUserAccess= function(service,name){
  queryargs = NULL
  body = NULL
  filepath = NULL
  return(ag_get(service = service, url = paste0(service$url,"users/",name,"/access"),queryargs = queryargs,body = body))
}

addUserAccess= function(service,name,read = FALSE,write = FALSE,catalog,repository){
  queryargs = list(read = read, write = write, catalog = catalog, repository = repository)
  body = NULL
  filepath = NULL
  invisible(ag_put(service = service, url = paste0(service$url,"users/",name,"/access"),queryargs = queryargs,body = body))
}

deleteUserAccess= function(service,name,read = FALSE,write = FALSE,catalog,repository){
  queryargs = list(read = read, write = write, catalog = catalog, repository = repository)
  body = NULL
  filepath = NULL
  invisible(ag_delete(service = service, url = paste0(service$url,"users/",name,"/access"),queryargs = queryargs,body = body))
}


