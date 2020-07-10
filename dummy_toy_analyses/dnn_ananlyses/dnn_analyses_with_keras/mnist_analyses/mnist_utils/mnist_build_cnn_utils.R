# ========================================
# Some Functions for Reuters dataset
# analyses
# ========================================

# Build Default Model
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
build_model_cnn <- function(input_shape) {
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = input_shape) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 10, activation = "softmax") %>%
    
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  return(model)
}


build_model_cnn_with_l2_reg <- function(input_shape, l2_reg_rate = 0.001) {
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                  input_shape = input_shape) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                  kernel_regularizer = regularizer_l2(l2_reg_rate)) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                  kernel_regularizer = regularizer_l2(l2_reg_rate))
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 64, activation = "relu",
                kernel_regularizer = regularizer_l2(l2_reg_rate)) %>%
    layer_dense(units = 10, activation = "softmax") %>%
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  
  return(model)
}


build_model_cnn_with_dropout <- function(input_shape, dropout_rate = 0.5) {
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                  input_shape = input_shape) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 10, activation = "softmax") %>%
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  return(model)
}


# Build Small Model
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
build_small_model_cnn <- function(input_shape) {
  
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 8, kernel_size = c(3, 3), activation = "relu",
                  input_shape = input_shape) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = "relu")
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 16, activation = "relu",
                kernel_regularizer = regularizer_l2(l2_reg_rate)) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 10, activation = "softmax") %>%
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}


build_small_model_cnn_with_dropout <- function(input_shape, dropout_rate = 0.5) {
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 8, kernel_size = c(3, 3), activation = "relu",
                  input_shape = input_shape) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = "relu")
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 10, activation = "softmax") %>%
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}


build_small_model_cnn_with_l2_reg <- function(input_shape, l2_reg_rate = 0.001) {
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 8, kernel_size = c(3, 3), activation = "relu",
                  input_shape = input_shape) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = "relu",
                  kernel_regularizer = regularizer_l2(l2_reg_rate)) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = "relu",
                  kernel_regularizer = regularizer_l2(l2_reg_rate))
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 16, activation = "relu",
                kernel_regularizer = regularizer_l2(l2_reg_rate)) %>%
    layer_dense(units = 10, activation = "softmax") %>%
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
}


# Build Information Bottleneck Model
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
build_information_bottleneck_model_cnn <- function(input_shape) {
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                  input_shape = input_shape) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 4, activation = "relu") %>%
    layer_dense(units = 10, activation = "softmax") %>%
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  return(model)
}


build_information_bottleneck_model_cnn_with_l2_reg <- function(input_shape, dropout_rate = 0.5) {
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                  input_shape = input_shape) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                  kernel_regularizer = regularizer_l2(l2_reg_rate)) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                  kernel_regularizer = regularizer_l2(l2_reg_rate))
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 4, activation = "relu",
                kernel_regularizer = regularizer_l2(l2_reg_rate)) %>%
    layer_dense(units = 10, activation = "softmax") %>%
    
    model %>% compile(
      optimizer = "rmsprop",
      loss = "categorical_crossentropy",
      metrics = c("accuracy")
    )
  
  return(model)
}


build_information_bottleneck_model_cnn_with_dropout <- function(input_shape, l2_reg_rate = 0.001) {
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                  input_shape = input_shape) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 4, activation = "relu") %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 10, activation = "softmax") %>%
    
    model %>% compile(
      optimizer = "rmsprop",
      loss = "categorical_crossentropy",
      metrics = c("accuracy")
    )
  return(model)
}

# End mnist_build_cnn_utils.R
