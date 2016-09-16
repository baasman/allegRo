
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



