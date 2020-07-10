plot.via.seaborn.pairplot.reg <- function(dat) {
  data_df <- data.frame(dat)
  colnames(data_df) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT")
  
  # Building a seaborn pairplot using pairplot()
  sns$pairplot(r_to_py(data_df), kind = "reg", corner = TRUE)
  #display the plot
  plt$show()
}

plot.via.seaborn.pairplot.diag.kde <- function(dat) {
  data_df <- data.frame(dat)
  colnames(data_df) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT")
  
  # Building a seaborn pairplot using pairplot()
  sns$pairplot(r_to_py(data_df), diag_kind = "kde", corner = TRUE)
  #display the plot
  plt$show()
}

show.joinplot.via.seaborn <- function(dat, attr.1, attr.2) {
  data_df <- data.frame(dat)
  colnames(data_df) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT")
  
  g <- sns$jointplot(attr.1, attr.2, data=data_df,
                kind="reg", truncate=FALSE,
                # xlim=c(0, 60), ylim=c(0, 12),
                color="b", height=7)
  plt$show()
}

plot.via.seaborn.pairplot.complex <- function(dat) {
  data_df <- data.frame(dat)
  colnames(data_df) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT")
  
  g = sns$PairGrid(data_df)
  
  g$map_upper(plt$scatter)
  g$map_lower(sns$kdeplot)
  g$map_diag(sns$kdeplot, lw=3, legend=FALSE);
  
  plt$show()
}


raw.plot.pairs <- function(dat) {
  dta <- data.frame(dat)
  colnames(dta) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT")
  
  dta.r <- abs(cor(dta)) # get correlations
  dta.col <- dmat.color(dta.r) # get colors
  # reorder variables so those with highest correlation
  # are closest to the diagonal
  dta.o <- order.single(dta.r)
  cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
         main="Variables Ordered and Colored by Correlation" )
}

raw.box.plot <- function(dat) {
  dta <- data.frame(dat)
  colnames(dta) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT")
  
  print(1:dim(dta)[[2]])
  
  return
  boxplot(
    dta[, 1], dta[, 2], dta[, 3], dta[, 4],
    dta[, 5], dta[, 6], dta[, 7], dta[, 8],
    dta[, 9], dta[, 10], dta[, 11], dta[, 12], dta[, 13],
    main = "Multiple boxplots for comparison",
    at = 1:dim(dta)[[2]],
    names = names(dta),
    las = 2,
    col = c("orange","red"),
    border = "brown",
    horizontal = TRUE,
    notch = FALSE # TRUE
  )
}

# End boston_housing_plot_util

# Links 
# (Example) -> https://seaborn.pydata.org/generated/seaborn.pairplot.html
# (All Tutorials Seaborn) -> https://seaborn.pydata.org/tutorial.html
# (Tutorial) -> https://seaborn.pydata.org/tutorial/axis_grids.html
