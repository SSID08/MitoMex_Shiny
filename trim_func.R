trim_func=function(df, col_name) {
  df[[col_name]] <- sapply(strsplit(df[[col_name]], ";"), function(x) {
    if (length(x) <= 5) {
      return(paste(x, collapse = ";"))
    } else {
      return(paste(x[1:5], collapse = ";"))
    }
  })
  return(df)
}