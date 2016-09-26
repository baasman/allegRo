
# all credit to user codeola on stackoverflow for this one!
expandUrlArgs <- function(x) structure(do.call("c", lapply(x, function(z) as.list(z))),
                                       names = rep(names(x), sapply(x, length)))

# convert to proper arguments
encodeURLArgs = function(arg) {
  if (isTRUE(arg)) {
    arg = "true"
  } else {
    arg = "false"
  }
}

#convert all logical vars in list to URL
convertLogical = function(args) {
  logArgs = lapply(args, mode)
  names = names(logArgs[logArgs == "logical"])
  args[names] = lapply(args[names], encodeURLArgs)
  return(args)
}

#check format for uploading files
checkFormat = function(filepath){
  if (grepl("nq", filepath) | grepl("nt", filepath)) {
    body = quote(upload_file(path = filepath, type = "text/plain"))
  } else if (grepl("rdf", filepath) | grepl("xml", filepath)) {
    body = quote(upload_file(path = filepath, type = "application/rdf+xml"))
  }
}

#' Create a geospatial literal
#' @description  Helper functions to create a cartesian literal using the type created with getCartesianGeoType or getSphericalGeoType
#' @param type Type created with getCartesianGeoType() or getSphericalGeoType()
#' @param x X coordinate
#' @param y Y coordinate
#' @param lat Latitude
#' @param long longitude
#' @param unit Defaults to 'degree'
#' @export
#' @name geoSpatialLiterals
#' @keywords literals,geospatial
createCartesianGeoLiteral = function(type,x,y){
  x = ifelse(x>=0,paste0("+",x),x)
  y = ifelse(y>=0,paste0("+",y),y)
  return(paste0('"',x,y,'"',"^^",type))
}



asISO = function(number,digits){
  if(nchar(number)>9){
    number = as.numeric(substr(number,1,9))
  }
  sign = "+"
  if(number<0){
    sign = "-"
    number = number*-1
  }
  floor = floor(number)
  return(paste0(sign,sprintf(sprintf("%%0%dd",digits),floor),".", stringr::str_split(stringr::str_pad(((number -floor)*10000000),7,pad = "0"),"\\.",2)[[1]][1]))
}




#' @rdname geoSpatialLiterals
#' @export
createSphericalGeoLiteral = function(type,lat,long,unit = 'degree'){
  if(unit=='degree'){
    conv = 1
  } else if(unit == "radian"){
    conv = 57.29577951308232
  } else if(unit == 'km'){
    conv = 0.008998159
  } else if(unit == 'mile'){
    conv = 0.014481134
  } else{
    stop("must input appropriate unit")
  }
  return(paste0('"',asISO(lat*conv,2),asISO(long*conv,3),'"^^',type))
}


#' Upload a data.table from a file
#' @description Upload a matrix in nquads format (matrix that is n x 3 or n x 4). Is it a wrapper around the
#' addStatementFile that efficiently writes out the triples to a temp file using data.table's fwrite, and
#' imports it into the repository
#' @param rep Repository object
#' @param triples data.table containing triples as three columns.
#' @param context graph name
#' @export
#'
uploadMatrix = function(rep,triples,context = NULL){

  stopifnot(packageVersion("data.table")>="1.9.7")
  dim = dim(triples)
  stopifnot(dim[2] %in% c(3,4))
  tmp = tempfile(pattern = "triples",tmpdir = tempdir(),fileext = ".nq")
  if(dim[2] == 4){
    if(triples[1,4,with = FALSE] != "."){
      stop("4th is not correct")
    }
  } else{
    triples[,"." := "."]
  }
  data.table::fwrite(x = triples,file.path = tmp,col.names = FALSE,sep = " ")

  addStatementsFromFile(rep,tmp,context = context)
}



