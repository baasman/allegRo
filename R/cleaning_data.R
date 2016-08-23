## convert to literal


removeXMLSchema = function(df){

  if(!("data.table" %in% class(df))) stop("To return as literals, please return as data.table for optimal performance")

  affectedCols = apply(dt,2,function(x) (sum(grepl("XMLSchema",x))>0))
  colnames = colnames(dt)
  for(col in colnames){
    dt[grepl("XML",get(col)), eval(parse(text = col)) := stringr::str_split_fixed(get(col),pattern = '"',3)[,2]]
  }
  return(dt)
}


