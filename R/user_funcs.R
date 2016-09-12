
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
listUsers = function(service){
  queryargs = NULL
  body = NULL
  url = paste0(service$url,"users")
  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' @rdname user
addUser = function(service,name,password){
  queryargs = list(password = password)
  body = NULL
  url = paste0(service$url,"users/",name)
  return(ag_put(service = service,url = url,queryargs = queryargs,body = body))
}

#' @rdname user
deleteUser = function(service,name){
  queryargs = NULL
  body = NULL
  url = paste0(service$url,"users/",name)
  return(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}

#' @rdname user
changeUserPassword = function(service,name, password){
  queryargs = NULL
  body = password
  filepath = NULL
  if(name == service$user){
    warning("You are changing the password of the current user. This will update your service object in the global environment.")
    newp = ag_post(service = service, url = paste0(service$url,"users/",service$user,"/password"),queryargs = NULL,body = body,filepath = NULL)$return
    service$password <<-  password
  } else{
    ag_post(service = service, url = paste0(service$url,"users/",name,"/password"),queryargs = NULL,body = body,filepath = NULL)$return
  }
  return(service)
}

