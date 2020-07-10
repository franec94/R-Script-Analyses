# ========================================
# Some Functions for IMDB dataset
# analyses
# ========================================

# ========================================
# Build Models Section
# ========================================
build_model <- function(input_shape) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = input_shape) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
}


build_model_with_dropout <- function(input_shape, dropout_rate = 0.5) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = input_shape) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
}


build_small_model <- function(input_shape) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu",
                input_shape = input_shape) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
}


build_small_model_with_dropout <- function(input_shape, dropout_rate = 0.5) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu",
                input_shape = input_shape) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
}


build_information_bottleneck_model <- function(input_shape) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = input_shape) %>%
    layer_dense(units = 4, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
}


build_information_bottleneck_model_with_dropout <- function(input_shape, dropout_rate = 0.5) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = input_shape) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 4, activation = "relu") %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
}

# ========================================
# General Purpose Utils
# ========================================
vectorize_sequences <- function(sequences, dimension = 10000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequence)) {
    results[i, sequences[[i]]] <- i
  }
  results
}


to_one_hot <- function(labels, dimension = 2) {
  results <- matrix(0, nrow = length(labels), ncol = dimension)
  for (i in 1:length(labels)) {
    results[i, labels[[i]]] <- i
  }
  results
}

# End imdb_reviews_build_utils.R
