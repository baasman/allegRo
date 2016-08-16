

#' Create a service object
#' @param url The url for your AllegroGraph server
#' @param user Username for user accessing store, if necessary
#' @param password Password for user accessing store, if necessary
#' @return S3 Object of type "service", which states the url, username, and password for the user
#' @export
#' @examples
#' createService("localhost","user","password")
createService = function(url,user = NULL,password = NULL){

  if(!is.character(url)& ! missing(url)) stop("url has to be supplied, and should be type character")
  if(!is.na(user) & !is.character(user)) stop("user should be a character value")
  if(!is.na(password) & !is.character(password)) stop("password should be a character value")

  structure(
    list(
      url = url,
      user = user,
      password = password
    ),
    class = "service"
  )
}
