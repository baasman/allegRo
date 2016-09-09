


#' Start a session to work on
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param autocommit Bolean. Specify whether or not the session should use transactions.
#' @param lifetime The number of seconds the session can be idle before being shutdown and reclaimed.
#' @param loadInitFile If true, then the initfile will be loaded when the session starts.
#' @param script The name of a script file to load; may be specified multiple times.
#'
#' @return A new service object that points to the newly created session, belonging to current user
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' startSession(service,repo = "testRepo",lifetime = 1000,script = "path/to/script")
#' }
startSession = function(service,catalogid = "root",repo = "",
                        autocommit = FALSE,lifetime = 100,loadInitFile = FALSE,
                        script = NULL){

  body = NULL
  filepath = NULL

  queryargs = convertLogical(list(autocommit = autocommit, lifetime = lifetime,loadInitFile = loadInitFile,
                   script = script))

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/session")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/session")
  }

  post = ag_post(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath)
  print(stringr::str_split_fixed(post$return,
                                 ":", 3)[, 3])

  serviceSession = createService(url = paste0(service$url,"session/",
                                              stringr::str_split_fixed(post$return,
                                              ":", 3)[, 3]),user = "baasman",password = "Aa20!bbb4")

  return(serviceSession)
}


commit = function(service){
  if(!grepl("session",service$url)) stop("Must use response from startSession to commit to repository")
  body = NULL
  filepath = NULL
  queryargs = NULL
  url = paste0(service$url,"commit")
  invisible(ag_post(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
}

closeSession = function(service){
   body = NULL
   filepath = NULL
   queryargs = NULL
   url = paste0(service$url,"session/close")
   invisible(ag_post(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
}

getDescriptionSession = function(service){
    body = NULL
    filepath = NULL
    queryargs = NULL
    url = paste0(service$url,"session/description")
    return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

isActiveSession = function(service){
  body = NULL
  filepath = NULL
  queryargs = NULL
  url = paste0(service$url,"session/isActive")
  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

pingSession = function(service){
  url = paste0(service$url,"session/isActive")
  return(ag_get(service = service,url = url,queryargs = NULL,body = NULL))
}

maintainSession = function(session){

}

x= "hey"
