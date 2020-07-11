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
  setwd("./mnist_analyses")
  },
  error=function(cond) {
    return(NA)
})


source("mnist_utils/mnist_setup_utils.R")

dataset <- dataset_mnist()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset

str(train_data)
str(train_targets)

str(test_data)
str(test_targets)

# Preprocess Dataset
# ----------------------------------------
train_data <- array_reshape(train_data, c(60000, 28 * 28))
train_data <- train_data / 255

test_data <- array_reshape(test_data, c(10000, 28 * 28))
test_data <- test_data / 255

train_targets <- to_categorical(train_targets)
test_targets <- to_categorical(test_targets)

input_shape <- c(28 * 28)
model <- build_model(input_shape = input_shape)
print(model)

result_train <- model %>% fit(
    train_data, train_targets,
    epochs = 5, batch_size = 128, verbose = 1) 
print(result_train)

metrics <- model %>% evaluate(test_data, test_targets, verbose = 1)
print(metrics)

classes_for_first_10_samples <- model %>% predict_classes(test_data[1:10, ])
print(classes_for_first_10_samples)

tryCatch({
  setwd("..")
},
error=function(cond) {
  return(NA)
})

# quit()
