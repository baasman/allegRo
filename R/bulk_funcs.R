
getBulkMode = function(service,catalogid = "root",repositoryid = ""){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/bulkMode")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid,"/bulkMode")
  }

  return(ag_get(service = service,url = url,queryargs = queryargs,body = body))
}

enableBulkMode = function(service,catalogid = "root",repositoryid = ""){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/bulkMode")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid,"/bulkMode")
  }

  return(ag_put(service = service,url = url,queryargs = queryargs,body = body))
}

disableBulkMode = function(service,catalogid = "root",repositoryid = ""){

  queryargs = NULL
  body = NULL

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/bulkMode")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid,"/bulkMode")
  }

  return(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}
