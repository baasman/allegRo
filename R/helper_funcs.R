
#all credit to user codeola on stackoverflow for this one!
expandUrlArgs <- function(x) structure(do.call("c", lapply(x, function(z) as.list(z))), names=rep(names(x), sapply(x, length)))

#convert to proper arguments
encodeURLArgs = function(arg){
  if(isTRUE(arg)){
    arg = "true"
  } else{
    arg = "false"
  }
}

convertLogical = function(args){
  logArgs = lapply(args,mode)
  names = names(logArgs[logArgs=="logical"])
  args[names] = lapply(args[names],encodeURLArgs)
  return(args)
}
