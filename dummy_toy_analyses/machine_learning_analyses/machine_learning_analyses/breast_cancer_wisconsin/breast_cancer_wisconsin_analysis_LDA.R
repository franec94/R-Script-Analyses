#!/usr/bin/env Rscript

# ========================================
# Resources: 
# Tutorial: https://towardsdatascience.com/linear-discriminant-analysis-lda-101-using-r-6a97217a55a6
# Dataset description: http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29
# ========================================

# ========================================
# Setup script & Activate Libraries
# ========================================

old_wd <- getwd()

tryCatch({
  setwd("breast_cancer_wisconsin")
},
error=function(cond) {
  return(NA)
})

getwd()

SEED = 1234
set.seed(seed = SEED)

source("./breast_cancer_wisconsin_utils/breast_cancer_wisconsin_setup_util.R")

# ========================================
# Start Script
# ========================================

# Fetch Input Data
wdbc <- read.csv("wdbc.csv", header = F)

features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(wdbc) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))

show.dataframe.stats(wdbc)

wdbc.data <- as.matrix(wdbc[,c(3:32)])
row.names(wdbc.data) <- wdbc$id
wdbc_raw <- cbind(wdbc.data, as.numeric(wdbc$diagnosis)-1)
colnames(wdbc_raw)[31] <- "diagnosis"

smp_size_raw <- floor(0.75 * nrow(wdbc_raw))
train_ind_raw <- sample(nrow(wdbc_raw), size = smp_size_raw)
train_raw.df <- as.data.frame(wdbc_raw[train_ind_raw, ])
test_raw.df <- as.data.frame(wdbc_raw[-train_ind_raw, ])

f <- paste(names(train_raw.df)[31], "~", paste(names(train_raw.df)[-31], collapse=" + "))
wdbc_raw.lda <- lda(as.formula(paste(f)), data = train_raw.df)

wdbc_raw.lda.predict <- predict(wdbc_raw.lda, newdata = test_raw.df)

### CONSTRUCTING ROC AUC PLOT:
# Get the posteriors as a dataframe.
wdbc_raw.lda.predict.posteriors <- as.data.frame(wdbc_raw.lda.predict$posterior)
# Evaluate the model
pred <- prediction(wdbc_raw.lda.predict.posteriors[,2], test_raw.df$diagnosis)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
# Plot
plot(roc.perf)
abline(a=0, b= 1)
text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

tryCatch({
  setwd(old_wd)
},
error=function(cond) {
  return(NA)
})

# quit()
