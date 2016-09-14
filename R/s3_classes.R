#' Create a service object
#'
#' @param url The url for your AllegroGraph server
#' @param user Username for user accessing store, if necessary
#' @param password Password for user accessing store, if necessary
#' @param testConnection Should the connection be tested? It can be tested seperately as well. Defaults to FALSE
#' @return S3 Object of type "service", which states the url, username, and password for the user
#' @export
#' @examples
#' \dontrun{
#' service("localhost","user","password",testConnection = FALSE)
#' }
service = function(url,user = NULL,password = NULL,testConnection = FALSE){

  if(!is.character(url)& ! missing(url)) stop("url has to be supplied, and should be type character")
  if(!missing(user) & !is.character(user)) stop("user should be a character value")
  if(!missing(password) & !is.character(password)) stop("password should be a character value")

  if(stringr::str_sub(url,start = -1) != "/"){
    url = paste0(url,"/")
  }

  obj = structure(
         list(
          url = url,
          user = user,
          password = password
          ),
        class = c("service","list")
    )

  if(testConnection){
    testConnection(obj)
    cat("Successful connection")
  }

  return(obj)
}


#' @export
print.service = function(x, ...){
  cat(paste0("Using port: ",gsub("[^0-9]","",x$url)))
  cat("\n \n")
  print(x["url"])
  print(x["user"])
  print(x["password"])
}

#' testConnection
#'
#' @param s The service object
#'
#' @return Print message stating whether or not the test was succesful
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' testConnection(service)
#' }
testConnection = function(s){
  if(http_error(GET(url = paste0(s["url"],"catalogs")))) stop("error: connection failed.
                                                              Check your service specifications
                                                              and/or ssh connection")
}



### CATALOG



#' Create a catalog object
#'
#' @param service Object of type service on which your catalog is located
#' @param catalog Character string specifying your catalog
#' @param testConnection Defaults to FALSE. If TRUE, will try to perform a simple command to test connection
#' @return S3 Object of type "catalog", which states the url, username, and password and catalog
#' @export
#' @examples
#' \dontrun{
#' service = service("localhost","user","password",testConnection = FALSE)
#' cat = catalog(service,"root")
#' }
catalog = function(service,catalog,testConnection = FALSE){

  if(!("service" %in% class(service))) stop("service object should be of class 'service'")
  if(!is.character(catalog)) stop("catalog has to be supplied, and should be a character")

  if(stringr::str_sub(catalog,start = -1) != "/"){
    catalogurl = paste0(catalog,"/")
  }

  if(catalog == "root"){
    catalogurl = service$url
  } else{
    catalogurl = paste0(service$url,"catalogs/",catalogurl)
  }

  obj = structure(
    list(
      url = catalogurl,
      user = service$user,
      password = service$password,
      catalog = catalog
    ),
    class = c("catalog","service")
  )

  if(testConnection){
    listRepositories(obj)
    cat("Successful connection to catalog:",catalog)
  }

  return(obj)
}

#' @export
print.catalog = function(x, ...){
  cat(paste0("Using port: ",gsub("[^0-9]","",x$url)))
  cat("\n \n")
  print(x["catalog"])
  print(x["user"])
  print(x["password"])

}


### REPOSITORY FUNCTIONS


#' Create a repository object
#'
#' @param catalog Object of type catalog on which your repository is located
#' @param repository Character string specifying your repository
#' @param testConnection Defaults to FALSE. If TRUE, will try to perform a simple command to test connection
#' @return S3 Object of type "repository", which states the url, username, password, catalog and repository you are working on
#' @export
#' @examples
#' \dontrun{
#' service = service("localhost","user","password",testConnection = TRUE)
#' cat = catalog(service,"root")
#' rep = repository(rep,"test")
#' }
repository = function(catalog,repository,testConnection = FALSE){

  if(!("catalog" %in% class(catalog))) stop("catalog should be of class 'catalog'")
  if(!is.character(repository)) stop("Repository should be a character")

  if(stringr::str_sub(repository,start = -1) != "/"){
    repourl = paste0(catalog$url,"repositories/",repository,"/")
  }

  obj = structure(
    list(
      url = repourl,
      user = catalog$user,
      password = catalog$password,
      catalog = catalog$catalog,
      repository = repository
    ),
    class = c("repository")
  )

  if(testConnection){
    getSize(obj)
    cat("Successful connection to repository:",repository)
  }

  return(obj)
}

#' @export
print.repository = function(x, ...){
  cat(paste0("Using port: ",gsub("[^0-9]","",x$url)))
  cat("\n \n")
  print(x["catalog"])
  print(x["repository"])
  print(x["user"])
  print(x["password"])
}

