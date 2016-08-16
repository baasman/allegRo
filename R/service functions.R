

#' List all catalogs
#' @param service Service object containing service url, username, and password
#' @return ag object
#' @export
#' @examples
#' listCatalogs(service)
listCatalogs = function(service){
  url = paste0(service$url,"catalogs")
  return(ag_get(service = service,url = url,queryargs = NULL))
}


#' List all catalogs
#' @param service Service object containing service url, username, and password
#' @param catalogid id for catalog of interest
#' @return ag object
#' @export
#' @examples
#' findProtocol(service)
findProtocol = function(service, catalogid = "root"){
  url = paste0(service$url,"catalogs/",catalogid,"/protocol")
  return(ag_get(service,url,NULL))
}

#' List all repositories in chosen catalog
#' @param service Service object containing service url, username, and password
#' @param catalogid id for catalog of interest
#' @return ag object
#' @export
#' @examples
#' listCatalogs(service)
listRepositories = function(service,catalogid = "root"){
  if(catalogid == "root"){
    url = paste0(service$url,"repositories")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories")
  }
  return(ag_get(service,url,NULL))
}


#' createRepository
#'
#' @param service Service object containing service url, username, and password
#' @param catalogid Id for catalog of interest
#' @param repositoryid Id for repository of interest
#' @param expectedSize Specifies the expected size of the repository
#' @param index Can be specified mulitple times. Should hold index names, and is used to configure the set of indices created for the store.
#' @param override Override repository, 1 or 0
#' @param restore Restore the repository
#' @param nocommit ...
#'
#' @return An object with the response and given url
#' @export
#'
#' @examples
#' createRepository(service,catalogid = "root",repositoryid = "test",
#' expectedSize = 100,index = NULL,override = "true",restore = NULL,nocommit = 1)
createRepository = function(service,catalogid = "root",repositoryid = "testFromR2",
                             expectedSize = NULL,index = NULL,override = c("true","false","if-not-open"),
                             restore = NULL,nocommit = c("1","0")){

  if(missing(override)) override = NULL
  if(missing(nocommit)) nocommit = NULL

  queryargs = list(expectedSize=expectedSize,index = index,override = override,restore = restore,nocommit = nocommit)
  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid)
  }
  body = NULL

  return(ag_put(service,url,queryargs,body))
}




#' Delete a Repository
#'
#' @param service Service object containing service url, username, and password
#' @param catalogid Id for catalog of interest
#' @param repositoryid Id for repository of interest
#' @return True or False, denoting whether delete was successful
#' @export
#'
#' @examples
#' deleteRepository(service,catalogid = "root",repositoryid = "repotodelete")
deleteRepository = function(service, catalogid = "root",repositoryid = "new_test2"){

  queryargs = NULL
  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid)
  }
  body = NULL
  return(ag_delete(service = service,url = url,queryargs = queryargs,body = body))
}


#get
listNameSpaces = function(service,catalogid= "root",repositoryid = "testfromr5"){
  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/namespaces")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,"/repositories/",repositoryid,"/namespaces")
  }
  return(ag_get(service,url,NULL,NULL))
}



getRepoAcess = function(service,catalogid = "root",repositoryid){
  if(catalogid == "root"){
    url = paste0(service@url,"repositories/",repositoryid,"/access")
  } else{
    url = paste0(service@url,"catalogs/",catalogid,
                 "/repositories/",repositoryid,"/access")
  }

  return(ag_get(service,url))
}


file = "C:/Users/baasman/Documents/testtrips.nq"

addStatementsFromFile = function(service,catalogid = "root",repositoryid = "testQueries",
                                  file,baseURI = NULL,context=NULL,commitEvery = NULL){

  if(missing(file)) stop("must supply path of file to be uploaded")

  queryargs = list(context = context,baseURI = baseURI,commit = commitEvery)

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid,"/statements")
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repositoryid,"/statements")
  }

  body = quote(upload_file(file,type = "text/plain"))

  invisible(ag_put)
}


query = "prefix emt:<http://montefiore.org/terminology#>
prefix upper:<http://montefiore.org/upper#>
prefix cdm:<http://montefiore.org/cdm#>
SELECT ?encounter (str(?hr) as ?hrv) ?timestamp
WHERE {   ?encounter a emt:InpatientEncounter .
?encounter cdm:measurement ?value .
?value upper:valueOf emt:Heartrate ;
upper:value ?hr ;
cdm:timeStamp/upper:value ?timestamp .   filter(?timestamp <= '2016-05-18T00:00:00-04:00'^^<http://www.w3.org/2001/XMLSchema#dateTime> && ?timestamp > '2016-05-17T00:00:00-04:00'^^<http://www.w3.org/2001/XMLSchema#dateTime> ) .

}"

query1 = "prefix emt:<http://montefiore.org/terminology#>
prefix upper:<http://montefiore.org/upper#>
prefix cdm:<http://montefiore.org/cdm#>
SELECT ?encounter (str(?hr) as ?hrv) ?timestamp
WHERE {   ?encounter a emt:InpatientEncounter .
?encounter cdm:measurement ?value .
?value upper:valueOf emt:Heartrate ;
upper:value ?hr ;
cdm:timeStamp/upper:value ?timestamp .   filter(?timestamp <= '2016-05-18T00:00:00-04:00'^^<http://www.w3.org/2001/XMLSchema#dateTime> && ?timestamp > '2016-05-17T00:00:00-04:00'^^<http://www.w3.org/2001/XMLSchema#dateTime> ) .

} limit 10000"

queryrdf = "select ?x ?y ?z { ?x ?y ?z} limit 10000"

evalQuery = function(service,catalogid = "root",repositoryid = "testfromr5",query,infer = NULL,context = NULL,
                     namedContext = NULL,callback = NULL,bindings = NULL,planner = NULL,checkVariables = NULL,
                     count = FALSE,accept = NULL,limit = 100){

  queryargs = list(query = query,limit = limit,infer = infer, context = context, namedContext = namedContext,
                   planner = planner, checkVariables = checkVariables)

  if(catalogid == "root"){
    url = paste0(service$url,"repositories/",repositoryid)
  } else{
    url = paste0(service$url,"catalogs/",catalogid,
                 "/repositories/",repositoryid)
  }

  invisible(ag_get(service = service,url = url,queryargs = queryargs,wantDF = 1))
}






