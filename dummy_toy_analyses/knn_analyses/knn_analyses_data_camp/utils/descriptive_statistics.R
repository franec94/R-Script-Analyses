# ============================================ #
# Libraries
# ============================================ #
all.libraries <- "D:/statistics/analyses/R-Script-Analyses/dummy_toy_analyses/knn_analyses/knn_analyses_data_camp/all_libraries.R"
source(all.libraries)


# ============================================ #
# Functions
# ============================================ #

scatter_plots.input.dataset <- function (data.df, columns) {
  pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = data.df, main = "Scatterplot Matrix")
  # scatterplot.matrix(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=data.df, main="Scatterplot Matrixs")
  # scatterplot(~dose+weight+gesttime+number, data=litter, main="Scatterplot Matrixs", labels=row.names(litter))
  
  # Scatterplot Matrices from the glus Package
  dta <- data.df[columns] # get data
  dta.r <- abs(cor(dta)) # get correlations
  dta.col <- dmat.color(dta.r) # get colors
  # reorder variables so those with highest correlation
  # are closest to the diagonal
  dta.o <- order.single(dta.r)
  cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
         main="Variables Ordered and Colored by Correlation" )
}
