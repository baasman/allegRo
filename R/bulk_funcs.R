
#' getBulkMode
#'
#' @name Bulkmode
#'
#' @param repository Object of type repository specifying server details and repository to work on.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' service = service("localhost","user","password")
#' rep = repository(catalog(service,"root"),"test")
#' getBulkMode(rep)
#' enableenableBulkMode(rep)
#' disableBdisableBulkMode(rep)
#' }
getBulkMode = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"/bulkMode")
  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

#' @rdname Bulkmode
#' @export
enableBulkMode = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"/bulkMode")
  return(ag_put(service = service,url = url,queryargs = queryargs,body = body))
}

#' @rdname Bulkmode
#' @export
disableBulkMode = function(repository){
  queryargs = NULL
  body = NULL
  url = paste0(repository$url,"/bulkMode")
  return(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}
