


#' Working with sessions
#'
#' @name Sessions
#'
#' @param Service Object of type repository specifying server details and repository to work on.
#' @param Session Session object created by the startSession function
#' @param autocommit Defaults to FALSE. Specify whether or not the session should use transactions.
#' @param lifetime The number of seconds the session can be idle before being shutdown and reclaimed.
#' @param loadInitFile If true, then the initfile will be loaded when the session starts.
#' @param script The name of a script file to load; may be specified multiple times.
#'
#' @return A new service object that points to the newly created session, belonging to current user
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' startSession(rep,lifetime = 1000,script = "path/to/script")
#' }
startSession = function(repository,
                        autocommit = FALSE,lifetime = 100,loadInitFile = FALSE,
                        script = NULL){

  body = NULL
  filepath = NULL

  queryargs = convertLogical(list(autocommit = autocommit, lifetime = lifetime,loadInitFile = loadInitFile,
                   script = script))


    url = paste0(repository$url,"session")


  post = ag_post(service = repository,url = url,queryargs = queryargs,body = body,filepath = filepath)
  serviceSession = service(url = paste0(url = service$url,"session/",
                                              stringr::str_split_fixed(post$return,
                                              ":", 3)[, 3]),user = service$user,password = service$password)

  invisible(serviceSession)
}

#' @rdname Sessions
commit = function(Session){
  if(!grepl("session",Session$url)) stop("Must use response from startSession to commit to repository")
  body = NULL
  filepath = NULL
  queryargs = NULL
  url = paste0(Session$url,"commit")
  invisible(ag_post(service = Session,url = url,queryargs = queryargs,body = body,filepath = filepath))
}

#' @rdname Sessions
closeSession = function(Session){
   body = NULL
   filepath = NULL
   queryargs = NULL
   url = paste0(Session$url,"session/close")
   invisible(ag_post(service = Session,url = url,queryargs = queryargs,body = body,filepath = filepath))
}

#' @rdname Sessions
getDescriptionSession = function(Session){
    body = NULL
    filepath = NULL
    queryargs = NULL
    url = paste0(Session$url,"session/description")
    return(ag_get(service = Session,url = url,queryargs = queryargs,body = body))
}

#' @rdname Sessions
isActiveSession = function(Session){
  body = NULL
  filepath = NULL
  queryargs = NULL
  url = paste0(Session$url,"session/isActive")
  return(ag_get(service = Session,url = url,queryargs = queryargs,body = body))
}

#' @rdname Sessions
pingSession = function(Session){
  url = paste0(Session$url,"session/isActive")
  return(ag_get(service = Session,url = url,queryargs = NULL,body = NULL))
}


