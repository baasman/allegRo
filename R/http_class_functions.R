#creates return object for get functions, and error checks the get request
ag_get = function(service, url,queryargs,body,cleanup = FALSE){


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
      response = resp
    ),
    class = "ag_get"
  )
}

### methods belonging to ag_get

#' @export
print.ag_get = function(x, ...){
  cat("Retrieved from AllegroGraph Server \n \n")
  if(length(x["return"])>0){
    if(!is.null(nrow(x[['return']]))) cat("Matrix dim: ",nrow(x[["return"]]),"x",ncol(x[["return"]]),"\n \n")
    if(is.null(nrow(x[["return"]]))){
      print(x[["return"]])
    } else{
      print(x$return[1:nrow(x$return),1:ncol(x$return)])
    }

  } else{
    print("Successful call but no content found")
  }
}





ag_statements = function(service, url,queryargs,body,returnType = NULL,cleanUp = FALSE,convert = FALSE){


  resp = GET(url,authenticate(service$user,service$password),body = eval(body), query = queryargs)

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

    if(returnType == "data.table"){
      ret = data.table::as.data.table(parsed)
      colnames(ret) = paste0("v",1:ncol(ret))
    } else if(returnType == "dataframe"){
      ret = as.data.frame(parsed,stringsAsFactors = FALSE)
      colnames(ret) = paste0("v",1:ncol(ret))
    } else if(returnType == "matrix"){
      ret = as.matrix(parsed)
      colnames(ret) = paste0("v",1:ncol(ret))
    } else{
      ret = parsed
    }

  } else if(http_type(resp) == "text/plain"){
    ret = content(resp,"text")
  }


  if(cleanUp){
    ret = removeXMLSchema(ret,convert = convert)
  }

  structure(
    list(
      return = ret,
      response = resp
    ),
    class = c("ag_statements","ag_data")
  )
}



ag_data = function(service, url,queryargs,body,returnType = NULL,cleanUp,convert = FALSE){

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

  if(grepl("ask",tolower(queryargs$query))){
    return(content(resp))
  } else if(grepl("describe",tolower(queryargs$query))){
      parsed = jsonlite::fromJSON(content(resp,"text"),simplifyVector = TRUE)
      if(is.integer(mean(unlist(lapply(lapply(parsed,as.list),length))))){
        ret = do.call(rbind,parsed)
        colnames(ret) = paste0("v",1:ncol(ret))
      } else{
        warning("uneven pattern lengths, can not converge to matrix")
        ret = parsed
        cleanUp = FALSE
      }
  } else if(http_type(resp) == "application/json"){
      parsed = jsonlite::fromJSON(content(resp,"text"),simplifyVector = TRUE)
      if(length(parsed$values)==0) stop("Query did not return any results")
      if(returnType == "data.table"){
        ret = data.table::as.data.table(parsed$values,col.names = parsed$names)
        colnames(ret) = parsed$names
      } else if(returnType == "dataframe"){
        ret = as.data.frame(parsed$values,stringsAsFactors = FALSE)
        colnames(ret) = parsed$names
      } else if(returnType == "matrix"){
        ret = as.matrix(parsed$values)
        colnames(ret) = parsed$names
      } else{
        ret = parsed$values
      }
  } else if(http_type(resp) == "text/plain"){
      parsed = content(resp,"text")
  }
  if(cleanUp){
    ret = removeXMLSchema(ret,convert = convert)
  }
  structure(
    list(
      return = ret,
      response = resp
    ),
    class = "ag_data"
  )
}

#' @export
print.ag_data = function(x, ...){
  cat("Retrieved from AllegroGraph Server \n")
  cat("First 10 results... \n \n")

  if(is.data.table(x[["return"]])){
    print(x[["return"]])
  } else if(nrow(x[["return"]])>10){
   print(x[["return"]][1:10,1:ncol(x[["return"]])])
  } else{
    print(x[["return"]])
  }
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
      resp = resp
    ),
    class = c("ag_put","list")
  )
}

### methods

#' @export
print.ag_put = function(x, ...){
  cat("Response from AllegroGraph server \n \n")

  print(x[["return"]])
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

  ret = content(resp)
  if(is.null(content(resp))) ret = TRUE

  structure(
    list(
      return = ret
    ),
    class = "ag_delete"
  )
}

#' @export
print.ag_delete = function(x, ...){
  cat("Successful response from AllegroGraph")
}




ag_post = function(service, url,queryargs,body,filepath,type = FALSE){

  if(type){
    #will add different encodings later
    content = ".json"
    encoding = "json"
    resp = POST(url,authenticate(service$user,service$password),body = eval(body),query = queryargs,
                encoding = encoding, content_type(content) )
  } else{
    resp = POST(url,authenticate(service$user,service$password),body = eval(body),query = queryargs )
  }

  if (http_error(resp) ) {
    stop(
      paste(content(resp))
      ,
      call. = FALSE
    )
  }

  structure(
    list(
      return = content(resp),
      url = url
    ),
    class = c("ag_post","list")
  )
}

### methods

#' @export
print.ag_post = function(x, ...){
  cat("Response from AllegroGraph server \n \n")

  print(x[["return"]])
}
