
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

uploadMatrix = function(rep,triples,context = NULL){

  stopifnot(packageVersion("data.table")>="1.9.7")
  dim = dim(triples)
  stopifnot(dim[2] == 3)
  tmp = tempfile(pattern = "triples",tmpdir = tempdir(),fileext = ".nq")
  if(dim[2] == 4){
    if(triples[1,4,with = FALSE] != "."){
      m[,"." := "."]
    }
  }
  data.table::fwrite(x = m,file.path = tmp,col.names = FALSE,sep = " ")

  addStatementsFromFile(rep,tmp,context = context)
}




a =  rep("<http:a>",1E6)
b =  rep("<http:b>",1E6)
c =  rep("<http:c>",1E6)

m = data.table(a,b,c)
