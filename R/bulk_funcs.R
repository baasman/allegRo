
#' getBulkMode
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#'
#' @return True if bulk mode is on, False otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' getBulkMode(service,catalogid = "root",repo = 'testRepo')
#' }
getBulkMode = function(service,catalogid = "root",repo = ""){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/bulkMode")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/bulkMode")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' enableBulkMode
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#'
#' @return ag put object
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' enableBulkMode(service,catalogid = "root",repo = 'testRepo')
#' }
enableBulkMode = function(service,catalogid = "root",repo = ""){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/bulkMode")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/bulkMode")
  }

  return(ag_put(service = service,url = url,queryargs = queryargs,body = body))
}

#' disableBulkMode
#'
#' @param service Service object containing service url, username, and password.
#' @param catalogid Id for catalog of interest.
#' @param repo Id for repository of interest.
#'
#' @return ag put object
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' disableBulkMode(service,catalogid = "root",repo = 'testRepo')
#' }
disableBulkMode = function(service,catalogid = "root",repo = ""){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repo,"/bulkMode")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repo,"/bulkMode")
  }

  return(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}
