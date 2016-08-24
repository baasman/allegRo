removeXMLSchema = function(df){

  if("data.table" %in% class(df)){
    affectedCols = apply(df,2,function(x) (sum(grepl("XMLSchema",x))>0))
    colnames = colnames(df)
    for(col in colnames){
      df[grepl("XMLSchema",get(col)), eval(parse(text = col)) := stringr::str_split_fixed(get(col),pattern = '"',3)[,2]]
    }
  } else if(("matrix" %in% class(df)) | ("data.frame" %in% class(df))){
    affectedCols = apply(df,2,function(x) (sum(grepl("XMLSchema",x))>0))
    colnames = colnames(df)
    for(col in colnames){
      ind = which(colnames(df) == col)
      df[grepl("XMLSchema",df[,ind]),ind] = stringr::str_split_fixed(df[grepl("XMLSchema",df[,ind]),ind],pattern = '"',3)[,2]
    }
  }

  return(df)
}


