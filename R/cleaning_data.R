


#remove xml schema and convert type
removeXMLSchema = function(df){

  if("data.table" %in% class(df)){
    affectedCols = apply(df,2,function(x) (sum(grepl("XMLSchema",x))>0))
    colnames = colnames(df)
    for(col in colnames){
      if(affectedCols[col]){
       type = stringr::str_split(df[[col]][1],pattern = "\\^\\^")[[1]][2]
       df[grepl("XMLSchema",get(col)), eval(col) := stringr::str_split_fixed(get(col),pattern = '"',3)[,2]]
       if(type == "<http://www.w3.org/2001/XMLSchema#int>"){
         set(x = df,j = col,value = as.numeric(df[[col]]))
       }
      }
    }
  } else if(("matrix" %in% class(df)) | ("data.frame" %in% class(df))){

    if(("matrix" %in% class(df))) warning("can't convert datatypes in matrix")

    affectedCols = apply(df,2,function(x) (sum(grepl("XMLSchema",x))>0))
    colnames = colnames(df)
    for(col in colnames){
      if(affectedCols[col]){
        ind = which(colnames(df) == col)
        type = stringr::str_split(df[,ind][1],pattern = "\\^\\^")[[1]][2]
        df[grepl("XMLSchema",df[,ind]),ind] = stringr::str_split_fixed(df[grepl("XMLSchema",df[,ind]),ind],pattern = '"',3)[,2]
        if(type == "<http://www.w3.org/2001/XMLSchema#int>" | type == "<http://www.w3.org/2001/XMLSchema#integer>"){
          df[,ind] = as.numeric(df[,ind])
        }
      }
    }
  }

  return(df)
}

