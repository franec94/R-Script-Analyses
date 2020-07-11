show.dataframe.stats <- function (dat) {
  
  if (is.na(dat) || is.null(dat)) {
    cat("input data frame either NA or NULL", "\n")
    return(NULL) 
  }
  str(dat)
  print(dim(dat))
  print(names(dat))
  
  head(dat)
  summary(dat)
}

# End breast_cancer_wisconsin_general_purpose_util.R
