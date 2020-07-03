#### #### #### #### #### #### #### #### #### #### #### 
#### 
#### Unbalanced data
#### https://shiring.github.io/machine_learning/2017/04/02/unbalanced
#### #### #### #### #### #### #### #### #### #### #### 


rm(list=ls())

## Librerie
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(mice)
library(caret)
library(ROSE)
library(DMwR)
library(mlbench)
library(ROCR)
# Directory
DIRDATA = "~/Desktop/statistico/lavori/lezioni/DataSpace/2018/Lez11/"
# I dati
bc_data <- read.table(paste(DIRDATA,"breast-cancer-wisconsin.data.txt",sep=""), 
                      header = FALSE, 
                      sep = ",")
summary(bc_data)                      
colnames(bc_data) <- c("sample_code_number", 
                       "clump_thickness", 
                       "uniformity_of_cell_size", 
                       "uniformity_of_cell_shape", 
                       "marginal_adhesion", 
                       "single_epithelial_cell_size", 
                       "bare_nuclei", 
                       "bland_chromatin", 
                       "normal_nucleoli", 
                       "mitosis", 
                       "classes")
bc_data$classes <- ifelse(bc_data$classes == "2", "benign",
                          ifelse(bc_data$classes == "4", "malignant", NA))                       
summary(bc_data) 

# Facciamo il table di classes
table(bc_data$classes)
table(bc_data$classes)/dim(bc_data)[1]

# facciamo qualche boxplot
par(mfrow=c(1,3))
boxplot(clump_thickness~classes, data=bc_data, col=2:3, main="clump_thickness")
boxplot(uniformity_of_cell_size~classes, data=bc_data, col=2:3, main="uniformity_of_cell_size")
boxplot(uniformity_of_cell_shape~classes, data=bc_data, col=2:3, main="uniformity_of_cell_shape")
par(mfrow=c(1,3))
boxplot(marginal_adhesion~classes, data=bc_data, col=2:3, main="marginal_adhesion")
boxplot(single_epithelial_cell_size~classes, data=bc_data, col=2:3, main="single_epithelial_cell_size")
#boxplot(bare_nuclei~classes, data=bc_data, col=2:3, main="bare_nuclei")
par(mfrow=c(1,3))
boxplot(bland_chromatin~classes, data=bc_data, col=2:3,main="bland_chromatin")
boxplot(normal_nucleoli~classes, data=bc_data, col=2:3, main="normal_nucleoli")
boxplot(mitosis~classes, data=bc_data, col=2:3, main="mitosis")


# vediamo gli NA

bc_data$bare_nuclei = as.character(bc_data$bare_nuclei)
bc_data$bare_nuclei[bc_data$bare_nuclei == "?"] = NA
bc_data$bare_nuclei = as.numeric(bc_data$bare_nuclei)


# potremmo eliminare gli na e avremmo 
nrow(bc_data[is.na(bc_data), ])/nrow(bc_data)
W = which(is.na(bc_data$bare_nuclei))
table(bc_data$classes[W])/length(W)

### due possibilità: eliminiamo o imputiamo i mancanti
# imputazione
set.seed(14)
dataset_impute <- mice(bc_data[, 2:10],  print = FALSE)
#bc_data <- cbind(bc_data[, 11, drop = FALSE], mice::complete(dataset_impute))
# eliminazione
bc_data = bc_data[!is.na(bc_data$bare_nuclei),]
# eliminiamo anche la prima colonna
bc_data = bc_data[,-1]

# # # # # # # # 
# Modelliamo i dati originali
# # # # # # # # 
set.seed(42)
index <- createDataPartition(bc_data$classes, p = 0.7, list = FALSE)
train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]


set.seed(42)
model_rf <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  verboseIter = FALSE))



final <- data.frame(actual = test_data$classes,
                    predict(model_rf, newdata = test_data, type = "prob"))
final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")
cm_original <- confusionMatrix(as.factor(final$predict), as.factor(test_data$classes))
?confusionMatrix
table(final$predict,test_data$classes)
# # # # # # # # # 
# under
# # # # # # # # # 
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")

set.seed(42)
model_rf_under <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = ctrl)


final_under <- data.frame(actual = test_data$classes,
                    predict(model_rf_under, newdata = test_data, type = "prob"))
final_under$predict <- ifelse(final_under$benign > 0.5, "benign", "malignant")
cm_under <- confusionMatrix(as.factor(final_under$predict), as.factor(test_data$classes))

## possiamo vedere se ci sono differenze
table(final_under$predict,final$predict)


# # # # # # # # # 
# over
# # # # # # # # # 

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "up")

set.seed(42)
model_rf_over <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = ctrl)
final_over <- data.frame(actual = test_data$classes,
                          predict(model_rf_over, newdata = test_data, type = "prob"))
final_over$predict <- ifelse(final_over$benign > 0.5, "benign", "malignant")
cm_over <- confusionMatrix(as.factor(final_over$predict), as.factor(test_data$classes))

## possiamo vedere se ci sono differenze
table(final_over$predict,final$predict)
table(final_over$predict,final_under$predict)

# # # # # # # # # # # # 
# Altri approcci: ROSE
# https://journal.r-project.org/archive/2014/RJ-2014-008/RJ-2014-008.pdf
# # # # # # # # # # # # 

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "rose")

set.seed(42)
model_rf_rose <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)
final_rose <- data.frame(actual = test_data$classes,
                         predict(model_rf_rose, newdata = test_data, type = "prob"))
final_rose$predict <- ifelse(final_rose$benign > 0.5, "benign", "malignant")
cm_rose <- confusionMatrix(as.factor(final_rose$predict), as.factor(test_data$classes))

## possiamo vedere se ci sono differenze
table(final_rose$predict,test_data$classes)
table(final_rose$predict,final_under$predict)

# # # # # # # # # # # # 
# Altri approcci: SMOTE
# https://arxiv.org/pdf/1106.1813.pdf
# # # # # # # # # # # # 

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(42)
model_rf_smote <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)
final_smote <- data.frame(actual = test_data$classes,
                         predict(model_rf_smote, newdata = test_data, type = "prob"))
final_smote$predict <- ifelse(final_smote$benign > 0.5, "benign", "malignant")
cm_smote <- confusionMatrix(as.factor(final_smote$predict), as.factor(test_data$classes))
## possiamo vedere se ci sono differenze
table(final_smote$predict,test_data$classes)
table(final_smote$predict,final_under$predict)


# # # # # # # # # # # # 
# Confrontiamo i valori
# # # # # # # # # # # # 

models <- list(original = model_rf,
                       under = model_rf_under,
                       over = model_rf_over,
                       smote = model_rf_smote,
                       rose = model_rf_rose)

results <- resamples(models)

# qualche plot
bwplot(results)

scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")

dotplot(results, scales=scales)

splom(results)

# testiamo la "significatività" dei modelli
diffs <- diff(results)
summary(diffs)


# # # # # # # # # # # # 
# Curve ROC (Prob benigno)
# # # # # # # # # # # # 
pred_final = prediction(final$benign, ifelse(test_data$classes=="benign",1,0 )) 
pred_under = prediction(final_under$benign, ifelse(test_data$classes=="benign",1,0 )) 
pred_over = prediction(final_over$benign, ifelse(test_data$classes=="benign",1,0 )) 
pred_rose = prediction(final_rose$benign, ifelse(test_data$classes=="benign",1,0 )) 
pred_smote = prediction(final_smote$benign,ifelse(test_data$classes=="benign",1,0 )) 

perf_final = performance(pred_final,"tpr","fpr")
perf_under = performance(pred_under,"tpr","fpr")
perf_over = performance(pred_over,"tpr","fpr")
perf_rose = performance(pred_rose,"tpr","fpr")
perf_smote = performance(pred_smote,"tpr","fpr")


par(mfrow=c(1,1))
plot(perf_final@x.values[[1]], perf_final@y.values[[1]],col=1, type="l", xlim=c(0,1), ylim=c(0,1), lwd=3)
lines(perf_under@x.values[[1]], perf_under@y.values[[1]],col=2, type="l", lwd=3)
lines(perf_over@x.values[[1]], perf_over@y.values[[1]],col=3, type="l", lwd=3)
lines(perf_rose@x.values[[1]], perf_rose@y.values[[1]],col=4, type="l", lwd=3)
lines(perf_smote@x.values[[1]], perf_smote@y.values[[1]],col=5, type="l", lwd=3)
abline(a=0,b=1)
legend("bottomright", c("final","under","over","rose","smote"), col=1:5, lwd=3)



# # # # # # # # # # # # 
# Curve ROC (Prob maligno)
# # # # # # # # # # # # 
pred_final = prediction(1-final$benign, ifelse(test_data$classes=="benign",0,1 )) 
pred_under = prediction(1-final_under$benign, ifelse(test_data$classes=="benign",0,1 )) 
pred_over = prediction(1-final_over$benign, ifelse(test_data$classes=="benign",0,1 )) 
pred_rose = prediction(1-final_rose$benign, ifelse(test_data$classes=="benign",0,1 )) 
pred_smote = prediction(1-final_smote$benign,ifelse(test_data$classes=="benign",0,1 )) 

perf_final = performance(pred_final,"tpr","fpr")
perf_under = performance(pred_under,"tpr","fpr")
perf_over = performance(pred_over,"tpr","fpr")
perf_rose = performance(pred_rose,"tpr","fpr")
perf_smote = performance(pred_smote,"tpr","fpr")


par(mfrow=c(1,1))
plot(perf_final@x.values[[1]], perf_final@y.values[[1]],col=1, type="l", xlim=c(0,1), ylim=c(0,1), lwd=3)
lines(perf_under@x.values[[1]], perf_under@y.values[[1]],col=2, type="l", lwd=3)
lines(perf_over@x.values[[1]], perf_over@y.values[[1]],col=3, type="l", lwd=3)
lines(perf_rose@x.values[[1]], perf_rose@y.values[[1]],col=4, type="l", lwd=3)
lines(perf_smote@x.values[[1]], perf_smote@y.values[[1]],col=5, type="l", lwd=3)
abline(a=0,b=1)
legend("bottomright", c("final","under","over","rose","smote"), col=1:5, lwd=3)



