#### #### #### #### #### #### #### #### #### #### #### 
#### 
#### Confronti tra Machine Learning approaches
#### https://machinelearningmastery.com/compare-the-performance-of-machine-learning-algorithms-in-r/
#### #### #### #### #### #### #### #### #### #### #### 


rm(list=ls())

# libraries
library(mlbench)
library(caret)

# load the dataset
data(PimaIndiansDiabetes)
#### Pima Indians Diabetes Database
# Variables: 
# pregnant  Number of times pregnant
# glucose  Plasma glucose concentration (glucose tolerance test)
# pressure  Diastolic blood pressure (mm Hg)
# triceps  Triceps skin fold thickness (mm)
# insulin  2-Hour serum insulin (mu U/ml)
# mass  Body mass index (weight in kg/(height in m)\^2)
# pedigree  Diabetes pedigree function
# age  Age (years)
# diabetes  Class variable (test for diabetes)

# descriptives
summary(PimaIndiansDiabetes)
plot(PimaIndiansDiabetes[,-9])
dim(PimaIndiansDiabetes)
# puliamo il dataset
for(i in c(2,3,4,7,8))
{
	W = PimaIndiansDiabetes[,i]!=0	
 	PimaIndiansDiabetes = PimaIndiansDiabetes[W,]
}
dim(PimaIndiansDiabetes)
plot(PimaIndiansDiabetes[,-9])

# boxplot
par(mfrow=c(1,4))
boxplot(pregnant ~diabetes, data=PimaIndiansDiabetes, col=2:3)
boxplot(glucose ~diabetes, data=PimaIndiansDiabetes, col=2:3)
boxplot(pressure ~diabetes, data=PimaIndiansDiabetes, col=2:3)
boxplot(triceps ~diabetes, data=PimaIndiansDiabetes, col=2:3)

par(mfrow=c(1,4))
boxplot(insulin ~diabetes, data=PimaIndiansDiabetes, col=2:3)
boxplot(mass ~diabetes, data=PimaIndiansDiabetes, col=2:3)
boxplot(pedigree ~diabetes, data=PimaIndiansDiabetes, col=2:3)
boxplot(age ~diabetes, data=PimaIndiansDiabetes, col=2:3)



# Creiamo il training set
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# CART
set.seed(7)
fit.cart <- train(diabetes~., data=PimaIndiansDiabetes, method="rpart", trControl=control)
# LDA
set.seed(7)
fit.lda <- train(diabetes~., data=PimaIndiansDiabetes, method="lda", trControl=control)
# SVM
set.seed(7)
fit.svm <- train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial", trControl=control)
# kNN
set.seed(7)
fit.knn <- train(diabetes~., data=PimaIndiansDiabetes, method="knn", trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(diabetes~., data=PimaIndiansDiabetes, method="rf", trControl=control)
# collect resamples
results <- resamples(list(CART=fit.cart, LDA=fit.lda, SVM=fit.svm, KNN=fit.knn, RF=fit.rf))


# facciamo un summary
summary(results)


# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)
#bwplot(results)

# density plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")


# dot plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
dotplot(results, scales=scales)


# parallel plots to compare models
parallelplot(results)


# pair-wise scatterplots of predictions to compare models
splom(results)

# xyplot plots to compare models
xyplot(results, models=c("LDA", "SVM"))


# difference in model predictions
diffs <- diff(results)
# ?diff.resamples
# summarize p-values for pair-wise comparisons
summary(diffs)