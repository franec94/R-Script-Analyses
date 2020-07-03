######################################################################
###
###  Decision tree -  Titanic
### The purpose of this dataset is to predict which people
### are more likely to survive after the collision with the iceberg. 
### The dataset contains 13 variables and 1309 observations. 
### The dataset is ordered by the variable X. 
### https://www.guru99.com/r-decision-trees.html#4                      
###
#######################################################################

###  Variable	Definition	Key
### survival	Survival		0 = No, 1 = Yes
### pclass	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
### sex		Sex	
### Age		Age in years	
### sibsp		# of siblings / spouses aboard the Titanic	
### parch		# of parents / children aboard the Titanic	
### ticket	Ticket number	
### fare		Passenger fare	
### cabin		Cabin number	
### embarked	Port of Embarkation	C = Cherbourg, Q = Queenstown, S = Southampton
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

rm(list=ls())

### Libraries
library(tree)
library(dplyr)
library(rpart.plot)
library(rpart)
library(ROCR)
library(randomForest)
library(caret)
library(e1071)
# Questa funzione installa una librerie che servirà per i random forest
#options(repos='http://cran.rstudio.org')
#have.packages <- installed.packages()
#cran.packages <- c('devtools','plotrix','randomForest','tree')
#to.install <- setdiff(cran.packages, have.packages[,1])
#if(length(to.install)>0) install.packages(to.install)

#library(devtools)
#if(!('reprtree' %in% installed.packages())){
#  install_github('araastat/reprtree')
#}
#for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))
library(reprtree)
### ### ### ### ### ### 

set.seed(678)
DIR     = "/Users/gianlucamastrantonio/Desktop/statistico/lavori/lezioni/DataSpace/2018/Lez7/"
Dataset = read.csv(paste(DIR,"titanic.csv",sep=""))
head(Dataset)
tail(Dataset)

# Puliamo il dataset
# 
DatasetClean <- Dataset %>%
  select(-c(home.dest, cabin, name, X, ticket)) %>% 
  #Convert to factor level
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  na.omit()
glimpse(DatasetClean)
summary(DatasetClean)
# dividiamo il campioni in train and test
shuffle_index = sample(1:nrow(DatasetClean))
head(shuffle_index)
DatasetShuffled = DatasetClean[shuffle_index,] 

### Creiamo una funzione per fare lo split
create_train_test  =  function(data, size = 0.8) {
	#
  n_row = nrow(data)
  total_row = round(size*n_row)
  train_sample =   1: total_row
  ret = list(Train = data[train_sample,],Test=data[-train_sample,])
  return(ret)
}
App =  create_train_test(DatasetShuffled, 0.8)
DataTrain = App$Train
DataTest = App$Test
dim(DataTrain)
dim(DataTest)
rm(App)

# controlliamo che la randomizzazione abbia funzionato
prop.table(table(DataTrain$survived))
prop.table(table(DataTest$survived))

# automatizziamo il processo

# versione 1
PropTable = function(x)
{
  prop.table(table(x))
}
apply(DataTrain[,c(1,2,3,5,6,8)],2,PropTable)
apply(DataTest[,c(1,2,3,5,6,8)],2,PropTable)


# versione 2
IndexVar = c(1,2,3,5,6,8)
for(i in IndexVar)
{
  print(colnames(DataTest)[i])
  print(prop.table(table(DataTrain[,i])))
  print(prop.table(table(DataTest[,i])))
}



# il modello
fit = rpart(survived~., data = DataTrain, method = 'class')
rpart.plot(fit, extra = 1,type=4)
rpart.plot(fit, extra = 106,type=4)
rpart.plot(fit, extra = 100,type=4, under=T)
#?rpart.plot  per vedere i vari tipi grafici
rpart.rules(fit)



# facciamo previsione (classe)
Pred = predict(fit, DataTest, type = 'class')
plot(Pred)
# Controlliamo la previsione
Tab =  table(DataTest$survived, Pred)
Tab
Accuracy = sum(diag(Tab))/sum(Tab)

# curva roc
# stimiamo le probs
PredProb = predict(fit, DataTest)
summary(PredProb)
pred = prediction(PredProb[,2], DataTest$survived) 
perf = performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
abline(a=0,b=1)

## vadiamo meglio come funziona la curca ROC
i = 5
a = perf@alpha.values[[1]][i]
fp = perf@x.values[[1]][i]
tp = perf@y.values[[1]][i]
glm.pred=rep("Down",252)
X = ifelse(PredProb[,2]>=a,"Yes","No")
table(X,DataTest$survived)
57/(37+51)
13/(108+13)

# tuning degli hyperparametri

# creaiamo uan funzione che mostre l'accuratezza
accuracy_tune = function(fit) {
  predict_unseen = predict(fit, DataTest, type = 'class')
  table_mat = table(DataTest$survived, predict_unseen)
  accuracy_Test = sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
# per fare il tuning dobbiamo settare 
# rpart.control(minsplit = 20, minbucket = round(20/3), maxdepth = 30)
# tra i molti parametri abbiamo
# -minsplit: Set the minimum number of observations in the node before the algorithm perform a split
# -minbucket:  Set the minimum number of observations in the final note i.e. the leaf
# -maxdepth: Set the maximum depth of any node of the final tree. The root node is treated a depth 0

# facciamo untest e vediamo come migliorare l'accuratezza
control = rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(survived~., data = DataTrain, method = 'class', control = control)
accuracy_tune(tune_fit)
# e vediamo la nuova roc
PredProbTune = predict(tune_fit, DataTest)
summary(PredProbTune)
predTune = prediction(PredProbTune[,2], DataTest$survived) 
perfTune = performance(predTune,"tpr","fpr")
plot(perfTune,colorize=TRUE)
abline(a=0,b=1)
plot(perf, add=T,colorize=TRUE, lty=2)


### ### ### ### ### ### ### ### ### ### ### 
### Random forest
### ### ### ### ### ### ### ### ### ### ### 

# modello base 
RF = randomForest(survived~. , ntree=10, mtry=1,data = DataTrain)
plot(RF)
RF
varImpPlot(RF)
# alcuni parametri
# ntree: number of trees in the forest
# mtry: Number of candidates draw to feed the algorithm. By default, it is the square of the number of columns.
# maxnodes: Set the maximum amount of terminal nodes in the forest
# importance=TRUE: Whether independent variables importance in the random forest be assessed

## plottiamo l'albero 2
reprtree:::plot.getTree(RF,k=2)


# Cerchiamo il miglior set di parametri

# troviamo mtry
tuneGrid <- expand.grid(mtry = c(1: 10))

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
rf_mtry <- train(survived~.,
                 data = DataTrain,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
rf_mtry

best_mtry <- rf_mtry$bestTune$mtry 

# cerchiamo il miglior maxnode
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in seq(6,30,by=3)) {
  #set.seed(1234)
  rf_maxnode <- train(survived~.,
                      data = DataTrain,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

# cerchiamo il miglior ntree
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(survived~.,
                       data = DataTrain,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 24,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)


# best model
fit_rf <- train(survived~.,
                DataTrain,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 800,
                maxnodes = 24)


# stimiamo il modello con random Forest
RFtot = randomForest(survived~. , ntree=800, maxnodes = 24,nodesize = 14,mtry=best_mtry,data = DataTrain)
varImp(RFtot)
varImpPlot(RFtot)

## plottiamo l'albero 2
reprtree:::plot.getTree(RFtot,k=2)

# facciamo previsione
prediction <-predict(RFtot, DataTest)

# possiamo vedere la matrice di confusione
confusionMatrix(prediction, DataTest$survived)

# curva ROC
PredProbRF = predict(fit, DataTest,  type="prob")
summary(PredProbRF)
predRF = prediction(PredProbRF[,2], DataTest$survived) 
perfRF = performance(pred,"tpr","fpr")
plot(perfRF,colorize=TRUE)
abline(a=0,b=1)
plot(perfTune,colorize=TRUE,add=T)

