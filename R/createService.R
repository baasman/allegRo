
###class specifications

#' Create a service object
#' @param url The url for your AllegroGraph server
#' @param user Username for user accessing store, if necessary
#' @param password Password for user accessing store, if necessary
#' @param testConnection Should the connection be tested? Defaults to FALSE
#' @return S3 Object of type "service", which states the url, username, and password for the user
#' @export
#' @examples
#' createService("localhost","user","password")
createService = function(url,user = NULL,password = NULL){

  if(!is.character(url)& ! missing(url)) stop("url has to be supplied, and should be type character")
  if(!is.na(user) & !is.character(user)) stop("user should be a character value")
  if(!is.na(password) & !is.character(password)) stop("password should be a character value")

  if(stringr::str_sub(url,start = -1) != "/"){
    url = paste0(url,"/")
  }

  structure(
         list(
          url = url,
          user = user,
          password = password
          ),
        class = c("service","list")
    )

}

###method declarations

testConnection = function(x) UseMethod("testConnection",x)

###methods


#' Summary of service object
#'
#' @param x Object of type service
#' @param ...
#'
#' @return Print statement showing all elements of service
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' summary(service)
#' }
summary.service = function(x, ...){
  cat("Service specifications: \n")
  print(x["url"])
  print(x["user"])
  print(x["password"])
}

#' testConnection
#'
#' @param x Your service object
#' @param ...
#'
#' @return Statement that states whether or not you can proceed with service object
#' @export
#'
#' @examples
#' \dontrun{
#' service = createService("localhost","user","password")
#' testConnection(service)
#' }
testConnection.service = function(x, ...){
  if(!http_error(GET(url = paste0(x["url"],"catalogs")))){
    print("Connection is successful, proceed...")
  } else{
    stop("error: connection failed. Check your service specifications and/or ssh connection")
  }
}


