#' getInitFile
#' @param service Service object containing service url, username, and password
#' @return ag get object containing init file
#' @export
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' getInitFile(service)
#' }
#' @import httr
getInitFile = function(service){
  queryargs = NULL
  body = NULL
  url = paste0(service$url,"initfile")
  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' setInitFile
#' @param service Service object containing service url, username, and password
#' @param filepath The filepath to your initFile (text)
#' @param restart Boolean. If true, then any running shared backends will be restarted.
#' @return ag put object: successful push or not
#' @export
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' setInitFile(service,filepath = "path/to/directory/initFile.txt")
#' }
#' @import httr
setInitFile = function(service,filepath,restart = "0"){

  if(missing(filepath)) stop("must supply path to txt file")
  if(tools::file_ext(filepath) != "txt") stop("file must be a txt file")

  queryargs = list(restart = restart)
  body = quote(upload_file(path = filepath,type = "text/plain"))

  url = paste0(service$url,"initfile")
  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
}

#' getVersion
#'
#' @param service Service object containing service url, username, and password
#'
#' @return Version of allegroGraph
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' getVersion(service)
#' }
#' @import httr
getVersion = function(service){
  queryargs = NULL
  body = NULL
  url = paste0(service$url,"version/info")
  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}



#' List all catalogs
#' @param service Service object containing service url, username, and password
#' @return ag object
#' @export
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' listCatalogs(service)
#' }
#' @import httr
listCatalogs = function(service){

  queryargs = NULL
  body = NULL

  url = paste0(service$url,"catalogs")

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}


