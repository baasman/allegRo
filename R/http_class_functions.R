#creates return object for get functions, and error checks the get request
ag_get = function(service, url,queryargs,wantDF){

  resp = GET(url,authenticate(service$user,service$password),query = queryargs )

  if (!(http_type(resp) %in% c("application/json","text/plain"))) {
    stop("API did not return proper format", call. = FALSE)
  }

  if(http_type(resp) == "application/json"){

    parsed = jsonlite::fromJSON(content(resp,"text"),simplifyVector = TRUE)
  } else if(http_type(resp) == "text/plain"){
    parsed = content(resp,"text")
  }

  if (http_error(resp) ) {
    stop(
      print(content(resp)),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      url = url,
      response = resp
    ),
    class = "ag_get"
  )
}




ag_put = function(service, url,queryargs,body){

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
      response = content(resp),
      url = url
    ),
    class = "ag_put"
  )
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

