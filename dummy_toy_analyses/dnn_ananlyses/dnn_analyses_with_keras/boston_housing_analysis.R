#!/usr/bin/env Rscript

# ========================================
# Setup script
# ========================================

if (length((ls())) != 0) {
  rm(list = ls())
}

# Close all images panels still 
if(length(dev.list()["RStudioGD"]) != 0) {
  dev.off(dev.list()["RStudioGD"])
}

# ========================================
# Activate Packages
# ========================================

library(reticulate)
use_virtualenv("r-tensorflow")
library(keras)

# reticulate::py_discover_config() 

#importing required Python libraries/modules
sns <- import('seaborn')
plt <- import('matplotlib.pyplot')
pd <- import('pandas')


# ========================================
# Define Some Functions
# ========================================

build_model <- function(input_shape) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = input_shape) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
}

# ========================================
# Begin Script
# ========================================

dataset <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset


train_data_df <- data.frame(train_data)

#building a seaborn pairplot using pairplot()
sns$pairplot(r_to_py(train_data_df))
#display the plot
plt$show()

str(train_data)
str(train_targets)

str(test_data)
str(test_targets)

mean.train_data <- apply(train_data, 2, mean)
std.train_data <- apply(train_data, 2, sd)

train_data <- scale(train_data, center = mean.train_data, scale = std.train_data)
test_data <- scale(test_data, center = mean.train_data, scale = std.train_data)




input_shape <- dim(train_data)[[2]]
model <- build_model(input_shape = input_shape)


history <- model %>%  fit(
  train_data,
  train_targets,
  epochs = 20,
  batch_size = 512,
)
plot(history)

predictions <- model %>% predict(test_data)
print(dim(predictions))

# quit()
