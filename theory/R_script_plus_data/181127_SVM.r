######################################################################
###
###  SVM -  cats
###
#######################################################################


## Libraries
library(e1071)
library(mlbench)
library(MASS)
library(ROCR)
# Working Directory
WD = "/Users/gianlucamastrantonio/Desktop/statistico/lavori/lezioni/DataSpace/2018/Lez8/"

# il dataset
data(cats, package="MASS")
?cats

# descrizione del dataset
dim(cats)
summary(cats)
plot(cats[,2],cats[,3], col=cats[,1], xlab= colnames(cats)[2],ylab= colnames(cats)[3], pch=20, cex=2)

# Creiamo training e testing 
set.seed(1)
cats[,"train"] 	= ifelse(runif(nrow(cats))<0.8,1,0)
trainset 		= cats[cats$train==1,]
testset 		= cats[cats$train==0,]
summary(trainset)
summary(testset)
# Eliminiamo la colonna "train"
trainColNum 	= grep("train",names(trainset))
trainset 		= trainset[,-trainColNum]
testset 		= testset[,-trainColNum]

# Trovare la colonna indicatrice 
typeColNum = grep("Sex",names(cats))

#modello kernel lineare
svm_model_lin = svm(Sex~ ., data=trainset, method="C-classification", kernel="linear", probability=T, scale=F)
svm_model_lin
# plot del svm
plot(svm_model_lin,trainset )

# previsioni
# train  set predictions
pred_train_lin <-predict(svm_model_lin,trainset)
mean(pred_train_lin==trainset$Sex)

#test set predictions
pred_test_lin <-predict(svm_model_lin,testset)
mean(pred_test_lin==testset$Sex)

#modello kernel radiale
svm_model_rad = svm(Sex~ ., data=trainset, method="C-classification", kernel="radial", probability=T,scale=F)
svm_model_rad
# plot del svm
plot(svm_model_rad,trainset )

# previsioni
# train  set predictions
pred_train_rad <-predict(svm_model_rad,trainset)
mean(pred_train_rad==trainset$Sex)

#test set predictions
pred_test_rad <-predict(svm_model_rad,testset)
mean(pred_test_rad==testset$Sex)



# Troviamo i migliori valori di gamma e cost
tune_out <- tune.svm(x=trainset[,-typeColNum],y=trainset[,typeColNum],gamma=10^(-3:3),cost=c(0.01,0.1,1,10,100,1000),kernel="radial")
summary(tune_out)
# salvimo i valori migliori
tune_out$best.parameters$cost
tune_out$best.parameters$gamma
# ristimiamo il modello
svm_model <- svm(Sex~ ., data=trainset, method="C-classification", kernel="radial",cost=tune_out$best.parameters$cost,gamma=tune_out$best.parameters$gamma , probability=T, scale=F)
# plot
plot(svm_model,trainset )
# Previsioni
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$Sex)
#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Sex)


# plot 
n_points_in_grid = 60 # dimensione della griglia
x_axis_range <- range (cats[, 2]) # range dell'asse x
y_axis_range <- range (cats[, 3]) # range dell'asse y

# griglie di valori
X_grid_points <- seq (from=x_axis_range[1], to=x_axis_range[2], length=n_points_in_grid)  
Y_grid_points <- seq (from=y_axis_range[1], to=y_axis_range[2], length=n_points_in_grid) 
# griglia estesa
all_grid_points <- expand.grid (X_grid_points, Y_grid_points) # generate all grid points
names (all_grid_points) <- c( "Bwt","Hwt") 
# facciamo previsione su tutta la griglia
all_points_predited <- predict(svm_model, all_grid_points, probability=T) 

# decidiamo il set dei colori
color_array <- c("red", "blue")[as.numeric(all_points_predited)] 
plot (all_grid_points[,c(2,1)], col=color_array, pch=20, cex=0.25)
points (x=trainset$Hwt, y=trainset$Bwt, col=c("red", "blue")[as.numeric(trainset$Sex)], pch=19) # plot data points
#points (trainset[svm_model$index, c ( 3,2)], pch=5, cex=2) # plot support vectors
points (svm_model$SV[,c(2,1)], pch=5, cex=2) 

# facciamo delle curve di livello per le prob di appartenenza classe
ProbGrid = attr(all_points_predited,"probabilities")[,2]
contour(Y_grid_points,X_grid_points, matrix(ProbGrid,n_points_in_grid, byrow=T),add=T, level=0.25, col=1)
contour(Y_grid_points,X_grid_points, matrix(ProbGrid,n_points_in_grid, byrow=T),add=T, level=0.5, col=2)
contour(Y_grid_points,X_grid_points, matrix(ProbGrid,n_points_in_grid, byrow=T),add=T, level=0.75, col=3)
contour(Y_grid_points,X_grid_points, matrix(ProbGrid,n_points_in_grid, byrow=T),add=T, level=0.90, col=4)


# ROC - training
PredProb_lin <-predict(svm_model_lin,trainset, probability=T)
PredProb_rad <-predict(svm_model_rad,trainset, probability=T)
PredProb_best <-predict(svm_model,trainset, probability=T)

# ROC - maschio
pred_lin = prediction(attr(PredProb_lin,"probabilities")[,2], ifelse(trainset$Sex=="M",1,0)) 
pred_rad = prediction(attr(PredProb_rad,"probabilities")[,2], ifelse(trainset$Sex=="M",1,0)) 
pred_best = prediction(attr(PredProb_best,"probabilities")[,2], ifelse(trainset$Sex=="M",1,0)) 
perf_lin = performance(pred_lin,"tpr","fpr")
perf_rad = performance(pred_rad,"tpr","fpr")
perf_best = performance(pred_best,"tpr","fpr")
plot(perf_lin,colorize=TRUE)
plot(perf_rad,colorize=TRUE, add=T, lty=2)
plot(perf_best,colorize=TRUE, add=T, lty=4)
abline(a=0,b=1)
legend("bottomright",c("lin", "rad","best"), col=1, lty=c(1,2,4))

# ROC - femmine
pred_lin = prediction(attr(PredProb_lin,"probabilities")[,1], ifelse(trainset$Sex=="F",1,0)) 
pred_rad = prediction(attr(PredProb_rad,"probabilities")[,1], ifelse(trainset$Sex=="F",1,0)) 
pred_best = prediction(attr(PredProb_best,"probabilities")[,1], ifelse(trainset$Sex=="F",1,0)) 
perf_lin = performance(pred_lin,"tpr","fpr")
perf_rad = performance(pred_rad,"tpr","fpr")
perf_best = performance(pred_best,"tpr","fpr")
plot(perf_lin,colorize=TRUE)
plot(perf_rad,colorize=TRUE, add=T, lty=2)
plot(perf_best,colorize=TRUE, add=T, lty=4)
abline(a=0,b=1)
legend("bottomright",c("lin", "rad","best"), col=1, lty=c(1,2,4))


