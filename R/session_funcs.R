


#' Working with sessions
#'
#' @name Sessions
#' @param catalog Object of type catalog specifying server details and catalog to work in
#' @param repo String that specifies what repository to start session on
#' @param Session Session object created by the startSession function
#' @param autocommit Defaults to FALSE. Specify whether or not the session should use transactions.
#' @param lifetime The number of seconds the session can be idle before being shutdown and reclaimed.
#' @param loadInitFile If true, then the initfile will be loaded when the session starts.
#' @param script The name of a script file to load; may be specified multiple times.
#'
#' @return A new repository object that points to the newly created session, belonging to current user. This session object can be substituted
#' for any function in which you were using the repository object.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' #Starting a session
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' session = startSession(rep,lifetime = 1000,script = "path/to/script")
#' }
#'
#' \dontrun{
#'
#' #Committing transactions to the repository
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' session = startSession(rep,lifetime = 1000)
#' commit(session)
#' }
#'
#' \dontrun{
#'
#' #Other useful functions
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' session = startSession(rep,lifetime = 100)
#' getDescriptionSession(session)
#' isActive(session)
#' pring(session)
#' closeSession(session)
#' }
startSession = function(catalog, repo,
                        autocommit = FALSE,lifetime = 100,loadInitFile = FALSE,
                        script = NULL){

  body = NULL
  filepath = NULL

  queryargs = convertLogical(list(autocommit = autocommit, lifetime = lifetime,loadInitFile = loadInitFile,
                   script = script))

  url = paste0(catalog$url,"repositories/",repo,"/session")

  post = ag_post(service = catalog,url = url,queryargs = queryargs,body = body,filepath = filepath)

  host = sub("rep.*","",catalog$url)
  host = sub("cat.*","",catalog$url)
  serviceSession = repository(catalog = catalog,repository = repo)
  serviceSession$url = paste0(host,"session/",
                       stringr::str_split_fixed(post$return,":", 3)[, 3],"/")

  invisible(serviceSession)
}


#function to test if session is alive
#' @rdname Sessions
#' @export
isActive = function(Session){
  active = NULL
  active = tryCatch({ag_get(service = Session,url = paste0(Session$url,"session/isActive"),queryargs = NULL,body = NULL)
                    active = TRUE},
           error = function(e) active = FALSE,
           warning = function(e) active = FALSE)
  return(active)
}



#' @rdname Sessions
#' @export
commit = function(Session){
  if(!grepl("session",Session$url)) stop("Must use response from startSession to commit to repository")
  stopifnot(isActive(Session))
  body = NULL
  filepath = NULL
  queryargs = NULL
  url = paste0(Session$url,"commit")
  invisible(ag_post(service = Session,url = url,queryargs = queryargs,body = body,filepath = filepath))
}


#' @rdname Sessions
#' @export
closeSession = function(Session){
   stopifnot(isActive(Session))
   body = NULL
   filepath = NULL
   queryargs = NULL
   url = paste0(Session$url,"session/close")
   invisible(ag_post(service = Session,url = url,queryargs = queryargs,body = body,filepath = filepath))
}

#' @rdname Sessions
#' @export
getDescriptionSession = function(Session){
    stopifnot(isActive(Session))
    body = NULL
    filepath = NULL
    queryargs = NULL
    url = paste0(Session$url,"session/description")
    return(ag_get(service = Session,url = url,queryargs = queryargs,body = body))
}


#' @rdname Sessions
#' @export
ping = function(Session){
  stopifnot(isActive(Session))
  url = paste0(Session$url,"session/isActive")
  return(ag_get(service = Session,url = url,queryargs = NULL,body = NULL))
}


