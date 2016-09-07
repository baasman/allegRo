
#all credit to user codeola on stackoverflow for this one!
expandUrlArgs <- function(x) structure(do.call(c, lapply(x, function(z) as.list(z))), names=rep(names(x), sapply(x, length)))
