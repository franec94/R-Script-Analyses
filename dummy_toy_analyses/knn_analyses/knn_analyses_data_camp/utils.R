get.iris.dataset <- function(url = NULL, columns = NULL, verbose = 0) {
  
  # Read in `iris` data
  if (is.null(url)) {
    iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), 
                     header = FALSE) 
  }
  
  
  # Add column names
  if (is.null(columns)) {
    names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  } else {
    names(iris) <- columns
  }
  
  # Print first lines
  if (verbose == 1) {
    # Return first 5 lines of `iris`
    print(head(iris))
    
    # Return structure of `iris`
    str(iris)
    
    # Summary overview of `iris`
    summary(iris) 
    
    # Refined summary overview
    summary(iris[c("Petal.Width", "Sepal.Width")])
  }
  
  # Return the result
  return(iris)
}

show.some.plots.data.iris <- function(iris) {
  
  p <- ggvis(iris, x = ~Sepal.Length, y = ~Sepal.Width)
  layer_points(p)
  
  # Iris scatter plot
  # iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
  
  # iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()
}


show.some.stats.data.iris <- function(iris) {
  # Overall correlation `Petal.Length` and `Petal.Width`
  cor(iris$Petal.Length, iris$Petal.Width)
  
  # Return values of `iris` levels 
  x=levels(iris$Species)
  
  # Print Setosa correlation matrix
  print(x[1])
  cor(iris[iris$Species==x[1],1:4])
  
  # Print Versicolor correlation matrix
  print(x[2])
  cor(iris[iris$Species==x[2],1:4])
  
  # Print Virginica correlation matrix
  print(x[3])
  cor(iris[iris$Species==x[3],1:4])
  
  # Division of `Species`
  table(iris$Species) 
  
  # Percentual division of `Species`
  round(prop.table(table(iris$Species)) * 100, digits = 1)
}

prepare.data.iris <- function(iris, ind, verbose = 0) {
  # Compose training set
  iris.training <- iris[ind==1, 1:4]
  
  # Inspect training set
  if (verbose == 1) {
    print(head(iris.training))
  }
  
  
  # Compose test set
  iris.test <- iris[ind==2, 1:4]
  
  # Inspect test set
  # Inspect training set
  if (verbose == 1) {
    print(head(iris.test))
  }
  
  # Compose `iris` training labels
  iris.trainLabels <- iris[ind==1,5]
  
  # Inspect result
  # Inspect training set
  if (verbose == 1) {
    print(iris.trainLabels)
  }
  
  # Compose `iris` test labels
  iris.testLabels <- iris[ind==2, 5]
  
  # Inspect result
  # Inspect training set
  if (verbose == 1) {
    print(iris.testLabels)
  }
  
  res <- list(iris.training=iris.training, iris.test=iris.test, iris.trainLabels=iris.trainLabels, iris.testLabels=iris.testLabels)
  return(res)
}

run.knn.analysis <- function(iris.training, iris.test, iris.trainLabels, iris.testLabels, verbose = 0) {
  # Build the model
  iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
  
  # Inspect `iris_pred`
  if (verbose == 1) {
    print(iris_pred)
  }
  
  
  # Put `iris.testLabels` in a data frame
  irisTestLabels <- data.frame(iris.testLabels)
  
  # Merge `iris_pred` and `iris.testLabels` 
  merge <- data.frame(iris_pred, iris.testLabels)
  
  # Specify column names for `merge`
  names(merge) <- c("Predicted Species", "Observed Species")
  
  # Inspect `merge` 
  if (verbose == 1) {
    print(merge)
  }
  
  CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)
}

train.knn.by.caret <- function(iris) {
  # Prepare data
  # -------------------------------------------------------------------
  # Create index to split based on labels  
  index <- createDataPartition(iris$Species, p=0.75, list=FALSE)
  
  # Subset training set with index
  iris.training <- iris[index,]
  
  # Subset test set with index
  iris.test <- iris[-index,]
  
  # -------------------------------------------------------------------
  # Overview of algos supported by caret
  names(getModelInfo())
  
  # Train a model
  model_knn <- train(iris.training[, 1:4], iris.training[, 5], method='knn')
  model_cart <- train(iris.training[, 1:4], iris.training[, 5], method='rpart2')
  
  # Predict the labels of the test set
  predictions<-predict(object=model_knn,iris.test[,1:4])
  
  # Evaluate the predictions
  table(predictions)
  
  # Confusion matrix 
  confusionMatrix(predictions,iris.test[,5])
  
  # Train the model with preprocessing
  model_knn <- train(iris.training[, 1:4], iris.training[, 5], method='knn', preProcess=c("center", "scale"))
  
  # Predict values
  predictions<-predict.train(object=model_knn,iris.test[,1:4], type="raw")
  
  # Confusion matrix
  confusionMatrix(predictions,iris.test[,5])
}
