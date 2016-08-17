#creates return object for get functions, and error checks the get request
ag_get = function(service, url,queryargs,body){

  resp = GET(url,authenticate(service$user,service$password),body = eval(body), query = queryargs )

  if (!(http_type(resp) %in% c("application/json","text/plain"))) {
    stop("API did not return proper format", call. = FALSE)
  }

  if (http_error(resp) ) {
    stop(
      print(content(resp)),
      call. = FALSE
    )
  }


  if(http_type(resp) == "application/json"){

    parsed = jsonlite::fromJSON(content(resp,"text"),simplifyVector = TRUE)
  } else if(http_type(resp) == "text/plain"){
    parsed = content(resp,"text")
  }

  structure(
    list(
      return = parsed,
      url = url,
      response = resp
    ),
    class = "ag_get"
  )
}

### methods belonging to ag_get



#' @export
print.ag_get = function(x, ...){
  cat("Retrieved from AllegroGraph Server \n \n")
  print(head(x["return"],10))
}





ag_data = function(service, url,queryargs,body,returnType = NULL){

  resp = GET(url,authenticate(service$user,service$password),body = eval(body), query = queryargs )

  if (!(http_type(resp) %in% c("application/json","text/plain"))) {
    stop("API did not return proper format", call. = FALSE)
  }

  if (http_error(resp) ) {
    stop(
      print(content(resp)),
      call. = FALSE
    )
  }

  if(http_type(resp) == "application/json"){

    parsed = jsonlite::fromJSON(content(resp,"text"),simplifyVector = TRUE)

    if(length(parsed$values)==0) stop("Query did not return any results")

    if(returnType == "dataframe"){
      ret = as.data.frame(parsed$values,col.names = parsed$names)
      colnames(ret) = parsed$names
    } else if(returnType == "matrix"){
      ret = as.matrix(parsed$values)
    } else{
      ret = parsed
    }

  } else if(http_type(resp) == "text/plain"){
    parsed = content(resp,"text")
  }


  structure(
    list(
      return = ret,
      url = url,
      response = resp
    ),
    class = "ag_data"
  )
}

#' @export
print.ag_data = function(x, ...){
  cat("Retrieved from AllegroGraph Server \n \n")
  print(head(x["return"],10))
}









ag_put = function(service, url,queryargs,body,filepath){

  resp = PUT(url,authenticate(service$user,service$password),body = eval(body),query = queryargs)

  if (http_error(resp) ) {
    stop(
      paste(content(resp))
      ,
      call. = FALSE
    )
  }

  structure(
    list(
      return = "Successful push",
      url = url
    ),
    class = c("ag_put","list")
  )
}

### methods

#' @export
print.ag_put = function(x, ...){
  cat("Response from AllegroGraph server \n \n")

  x["return"]
}




ag_delete = function(service, url,queryargs,body){

  resp = DELETE(url,authenticate(service$user,service$password),body = eval(body),query = queryargs)

  if (http_error(resp) ) {
    stop(
      paste(content(resp))
      ,
      call. = FALSE
    )
  }

  structure(
    list(
      response = content(resp),
      url = url
    ),
    class = "ag_delete"
  )
}

