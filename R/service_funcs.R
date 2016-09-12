#' Find the contents of the Initialization file
#'
#' @description Retrieve the server's initialization file.
#' The initialization-file is a collection of Common Lisp code that is executed on every shared back-end when it starts
#'
#' @param service Service object containing service url, username, and password
#' @return The text contained in your Initialization file
#' @export
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' getInitFile(service)
#' }
#' @import httr
getInitFile = function(service){
  queryargs = NULL
  body = NULL
  url = paste0(service$url,"initfile")
  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' Set the initialization file
#'
#' @description Replace the current initialization file
#'
#' @param service Service object containing service url, username, and password
#' @param filepath The filepath to your initFile (text)
#' @param restart Defaults to FALSE. If TRUE, then any running shared backends will be restarted. This ensures that subsequent requests will use the new initialization file.
#' @return ag put object stating the success of the push, or not (invisible)
#' @export
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' setInitFile(service,filepath = "path/to/directory/initFile.txt")
#' }
#' @import httr
setInitFile = function(service,filepath,restart = FALSE){

  if(missing(filepath)) stop("must supply path to txt file")
  if(tools::file_ext(filepath) != "txt") stop("file must be a txt file")

  queryargs = list(restart = restart)
  body = quote(upload_file(path = filepath,type = "text/plain"))

  url = paste0(service$url,"initfile")
  invisible(ag_put(service = service,url = url,queryargs = queryargs,body = body,filepath = filepath))
}


#' Find the AllegroGraph server version
#'
#' @description Returns the version of the AllegroGraph server, as a string.
#'
#' @param service Service object containing service url, username, and password
#' @param complex Default FALSE. Will return all version related info. If FALSE, will just return version number
#'
#' @return Version of allegroGraph
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' getVersion(service)
#' }
#' @import httr
getVersion = function(service, complex = FALSE){
  queryargs = NULL
  body = NULL
  if(complex){
    url = paste0(service$url,"version/info")
  } else{
    url = paste0(service$url,"version")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}


#' List all catalogs
#'
#' @description Returns a set of catalogs that are available on this server.
#'
#' @param service Service object containing server url, username, and password
#' @return For each catalog, id and uri properties are returned, giving respectively the name of the catalog and the URL under which it is found.
#' Properties named readable and writable indicate, for each catalog, whether the current user has read or write access to it.
#' @export
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' listCatalogs(service)
#' }
#' @import httr
listCatalogs = function(service){

  queryargs = NULL
  body = NULL

  url = paste0(service$url,"catalogs")

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}




