
###class specifications

#' createService
#'
#' @param url The url for your AllegroGraph server
#' @param user Username for user accessing store, if necessary
#' @param password Password for user accessing store, if necessary
#' @param testConnection Should the connection be tested? It can be tested seperately as well. Defaults to FALSE
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


#' @export
print.service = function(x, ...){
  cat("Service specifications: \n")
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




# createService = function(url,user = NULL,password = NULL,testConnection = FALSE){
#
#   if(!is.character(url)& ! missing(url)) stop("url has to be supplied, and should be type character")
#   if(!is.na(user) & !is.character(user)) stop("user should be a character value")
#   if(!is.na(password) & !is.character(password)) stop("password should be a character value")
#
#   if(stringr::str_sub(url,start = -1) != "/"){
#     url = paste0(url,"/")
#   }
#
#   obj = structure(
#     list(
#       url = url,
#       user = user,
#       password = password
#     ),
#     class = c("service","list")
#   )
#
#   if(testConnection){
#     tryCatch(
#       {
#         testConnection(obj)
#         p = "produced no error!"
#       },
#       warning = function(cond){
#         message("There seemed to be a problem with the connection.")
#         message("The error message: ",content(resp))
#         p = "produced an error"
#       },
#       finally = {
#         message("server url - ",obj["url"]," ",p)
#       }
#     )
#   }
#
#   invisible(obj)
# }
