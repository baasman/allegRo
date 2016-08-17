
###class specifications

#' Create a service object
#' @param url The url for your AllegroGraph server
#' @param user Username for user accessing store, if necessary
#' @param password Password for user accessing store, if necessary
#' @param testConnection Should the connection be tested? Defaults to FALSE
#' @return S3 Object of type "service", which states the url, username, and password for the user
#' @export
#' @examples
#' createService("localhost","user","password",testConnection = FALSE)
createService = function(url,user = NULL,password = NULL,testConnection = FALSE){

  if(!is.character(url)& ! missing(url)) stop("url has to be supplied, and should be type character")
  if(!is.na(user) & !is.character(user)) stop("user should be a character value")
  if(!is.na(password) & !is.character(password)) stop("password should be a character value")

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
  }

  return(obj)
}

###method declarations

testConnection = function(x) UseMethod("testConnection",x)


#' @export
summary.service = function(x, ...){
  cat("Service specifications: \n")
  print(x["url"])
  print(x["user"])
  print(x["password"])
}

#' testConnection
#'
#' @param x The service object
#' @param ...
#'
#' @return Print message stating whether or not the test was succesful
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' testConnection(service)
#' }
testConnection = function(x, ...){
  if(!http_error(GET(url = paste0(x["url"],"catalogs")))){
    print("Connection is successful, proceed...")
  } else{
    stop("error: connection failed. Check your service specifications and/or ssh connection")
  }
}


