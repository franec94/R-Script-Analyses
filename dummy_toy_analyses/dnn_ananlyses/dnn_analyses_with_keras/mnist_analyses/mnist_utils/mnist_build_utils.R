# ========================================
# Some Functions for Reuters dataset
# analyses
# ========================================

# Build Default Model
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
build_model <- function(input_shape) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 512, activation = "relu",
                input_shape = input_shape) %>%
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}


build_model_with_l2_reg <- function(input_shape, l2_reg_rate = 0.001) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = input_shape) %>%
  layer_dense(units = 64, activation = "relu",
              kernel_regularizer = regularizer_l2(l2_reg_rate)) %>%
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
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
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}


# Build Small Model
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
build_small_model <- function(input_shape) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu",
                input_shape = input_shape) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}


build_small_model_with_dropout <- function(input_shape, dropout_rate = 0.5) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu",
                input_shape = input_shape) %>%
    layer_dense(units = 16, activation = "relu",
                kernel_regularizer = regularizer_l2(l2_reg_rate)) %>%
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}


build_small_model_with_l2_reg <- function(input_shape, l2_reg_rate = 0.001) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu",
                input_shape = input_shape) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}


# Build Information Bottleneck Model
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
build_information_bottleneck_model <- function(input_shape) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = input_shape) %>%
    layer_dense(units = 4, activation = "relu") %>%
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}


build_information_bottleneck_model_with_l2_reg <- function(input_shape, dropout_rate = 0.5) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = input_shape) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 4, activation = "relu") %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}


build_information_bottleneck_model_with_dropout <- function(input_shape, l2_reg_rate = 0.001) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = input_shape) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 4, activation = "relu",
                kernel_regularizer = regularizer_l2(l2_reg_rate)) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}

# End bostom_housing_train_utils.R
