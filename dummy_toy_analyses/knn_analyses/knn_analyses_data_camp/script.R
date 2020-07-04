#!/usr/bin/env Rscript

# ========================================
# Setup script
# ========================================
if (length((ls())) != 0) {
  rm(list = ls())
}

# Close all images panels still opened
dev.off(dev.list()["RStudioGD"])


# ========================================
# Import Libraries
# ========================================
getwd()

all.libraries <- "D:/statistics/analyses/R-Script-Analyses/dummy_toy_analyses/knn_analyses/knn_analyses_data_camp/all_libraries.R"
source(all.libraries)

utils.src <- "D:/statistics/analyses/R-Script-Analyses/dummy_toy_analyses/knn_analyses/knn_analyses_data_camp/utils.R"
source(utils.src)

utils.descriptive_statistics.src <- "D:/statistics/analyses/R-Script-Analyses/dummy_toy_analyses/knn_analyses/knn_analyses_data_camp/utils/descriptive_statistics.R"
source(utils.descriptive_statistics.src)

# Global Variables
# ----------------------------------------
SEED = 1234

set.seed(seed = SEED)

# Load Dataset
iris <- get.iris.dataset(verbose = 1)
attach(iris)

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# ========================================
# Data Investigation Step 
# ========================================

show.some.plots.data.iris(iris)
# show.some.stats.data.iris(iris)

scatter_plots.input.dataset(iris, c(1,2,3,4))

# ========================================
# Train Step
# ========================================

# Build your own `normalize()` function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalize the `iris` data
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))

# Summarize `iris_norm`
summary(iris_norm)

res <- prepare.data.iris(iris, ind, verbose = 1)
str(res)

iris.training <- res$iris.training
iris.test <- res$iris.test
iris.trainLabels <- res$iris.trainLabels
iris.testLabels <- res$iris.testLabels

# run.knn.analysis(iris.training, iris.test, iris.trainLabels, iris.testLabels, verbose = 0)

train.knn.by.caret(iris)

# quit()
