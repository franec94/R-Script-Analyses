#!/usr/bin/env Rscript

# ========================================
# Resources: 
# Dataset description: https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html
# ========================================

# ========================================
# Setup script & Activate Libraries
# ========================================
SEED = 1234
set.seed(seed = SEED)

tryCatch({
  setwd("./boston_housing_analyses")
  },
  error=function(cond) {
    return(NA)
})


source("boston_housing_utils/boston_housing_setup_utils.R")

# ========================================
# Define Some Functions
# ========================================

# ========================================
# Begin Script
# ========================================

# Load Dataset 
# and check whether everything is ok
# ----------------------------------------
dataset <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset

str(train_data)
str(train_targets)

str(test_data)
str(test_targets)

# Preprocess Dataset
# ----------------------------------------
mean.train_data <- apply(train_data, 2, mean)
std.train_data <- apply(train_data, 2, sd)

train_data <- scale(train_data, center = mean.train_data, scale = std.train_data)
test_data <- scale(test_data, center = mean.train_data, scale = std.train_data)

# Train Models
# ----------------------------------------

# Train Default Model
# train_default_model(train_data, test_data, train_targets, test_targets)

# k.fold.crossval(train_data, train_targets, k = 4, epochs = 100)
# k.fold.crossval.with.plots(train_data, train_targets, k = 4, epochs = 500)

# Train Final Model
input_shape <- dim(train_data[[2]])
model <- build_model(input_shape = input_shape)

for (batch_size in c(8, 16, 32, 64, 128, 256, 512)) {
  cat("Training with entire train set, with batch size: ", batch_size, ".\n")
  
  result_train <- model %>% fit(
    train_data, train_targets,
    epochs = 80, batch_size = batch_size, verbose = 0) # batch_size = 16
  print("Train Results")
  print(result_train)
  
  print("Test Results")
  results_eval <- model %>% evaluate(test_data, test_targets, verbose = 0)
  print(results_eval)
}

tryCatch({
  setwd("..")
},
error=function(cond) {
  return(NA)
})

# quit()
