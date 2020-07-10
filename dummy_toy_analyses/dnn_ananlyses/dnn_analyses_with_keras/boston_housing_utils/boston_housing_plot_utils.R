plot.via.seaborn.pairplot.reg <- function(dat) {
  data_df <- data.frame(dat)
  colnames(data_df) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "LSTAT")
  
  # Building a seaborn pairplot using pairplot()
  sns$pairplot(r_to_py(data_df), kind = "reg", corner = TRUE)
  #display the plot
  plt$show()
}

plot.via.seaborn.pairplot.diag.kde <- function(dat) {
  data_df <- data.frame(dat)
  colnames(data_df) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "LSTAT")
  
  # Building a seaborn pairplot using pairplot()
  sns$pairplot(r_to_py(data_df), diag_kind = "kde", corner = TRUE)
  #display the plot
  plt$show()
}

show.joinplot.via.seaborn <- function(dat, attr.1, attr.2) {
  data_df <- data.frame(dat)
  colnames(data_df) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "LSTAT")
  
  g <- sns$jointplot(attr.1, attr.2, data=data_df,
                kind="reg", truncate=FALSE,
                # xlim=c(0, 60), ylim=c(0, 12),
                color="b", height=7)
  plt$show()
}

# End boston_housing_plot_util

# Links 
# (Example) -> https://seaborn.pydata.org/generated/seaborn.pairplot.html
