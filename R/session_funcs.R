


#' startSession
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#' @param autocommit Bolean. Specify whether or not the session should use transactions.
#' @param lifetime The number of seconds the session can be idle before being shutdown and reclaimed.
#' @param loadInitFile If true, then the initfile will be loaded when the session starts.
#' @param script The name of a script file to load; may be specified multiple times.
#'
#' @return ag_post object containing the url of the session on the server
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' startSession(service,repo = "testRepo",lifetime = 1000,script = "path/to/script")
#' }
startSession = function(service,catalogid = "root",repo = "testFromR2",
                        autocommit = NULL,lifetime = 100,loadInitFile = NULL,
                        script = NULL){

  body = NULL
  filepath = NULL

  queryargs = list(autocommit = autocommit, lifetime = lifetime,loadInitFile = loadInitFile,
                   script = script)

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/session")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/session")
  }

  return(ag_post(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
}


# commit = function(service,catalogid = "root",repo = "",session = NULL){
#
#   body = NULL
#   filepath = NULL
#   queryargs = NULL
#
#   if(catalogid == "root"){
#     url = paste0(service$url,"commit")
#   } else{
#     url = paste0(service$url,"catalogs/",catalogid,"/commit")
#   }
#
#   return(ag_post(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
# }



#
# closeSession = function(service,catalogid = "root",repo = "testFromR2",session){
#
#   body = NULL
#   filepath = NULL
#   queryargs = NULL
#
#   port = gsub(".*:\\s*|/.*","",session$return)
#   uid = stringr::str_split_fixed(session$return,":",3)[3]
#
#
#   if(catalogid == "root"){
#     url = paste0(service$url,"repositories/",repo,"/session/",uid,"/close")
#   } else{
#     url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/session/",uid,"/close")
#   }
#
#
#   invisible(ag_post(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
# }
#
# getDescSession = function(service,catalogid = "root",repo = "testFromR2",session){
#
#   body = NULL
#   queryargs = NULL
#
#   port = gsub(".*:\\s*|/.*","",s)
#
#   if(catalogid == "root"){
#     url = paste0(service$url,"repositories/",repo,"/session/",)
#   } else{
#     url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/session/description")
#   }
#
#   invisible(ag_get(service = service,url = url,queryargs = queryargs,body = body))
# }
