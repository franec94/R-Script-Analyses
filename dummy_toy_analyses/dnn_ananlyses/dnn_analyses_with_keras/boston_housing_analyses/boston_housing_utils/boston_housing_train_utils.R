# source("boston_housing_build_utils.R")

# Train Default Model
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
train_default_model <- function(train_data, test_data, train_targets, test_targets) {
  input_shape <- dim(train_data)[[2]]
  model <- build_model(input_shape = input_shape)
  
  history <- model %>%  fit(
    train_data,
    train_targets,
    epochs = 20,
    batch_size = 512,
  )
  plot(history)
  
  results <- model %>% evaluate(test_data, test_targets)
  print(results)
  
  predictions <- model %>% predict(test_data)
  print(dim(predictions))
  
  input_shape <- dim(train_data)[[2]]
  model <- build_model_with_l2_reg(input_shape = input_shape)
  
  history <- model %>%  fit(
    train_data,
    train_targets,
    epochs = 20,
    batch_size = 512,
  )
  plot(history)
  
  predictions <- model %>% predict(test_data)
  print(dim(predictions))
  
  results <- model %>% evaluate(test_data, test_targets)
  print(results)
  
  input_shape <- dim(train_data)[[2]]
  model <- build_model_with_l2_reg(input_shape = input_shape)
  
  history <- model %>%  fit(
    train_data,
    train_targets,
    epochs = 20,
    batch_size = 512,
  )
  plot(history)
  
  results <- model %>% evaluate(test_data, test_targets)
  print(results)
  
  predictions <- model %>% predict(test_data)
  print(dim(predictions)) 
}


k.fold.crossval.with.plots <- function(train_data, train_targets, k = 4, epochs = 50) {
  indeces <- sample(1:nrow(train_data))
  folds <- cut(indeces, breaks = k, labels = FALSE)
  
  
  all_mae_histories <- NULL
  for (i in 1:k) {
    cat("processing fold #", i, "\n")
    
    val_indeces <- which(folds == i, arr.ind = TRUE)
    val_data <- train_data[val_indeces, ]
    val_targets <- train_targets[val_indeces]
    
    partial_train_data <- train_data[-val_indeces, ]
    partial_train_targets <- train_targets[-val_indeces]
    
    input_shape <- dim(train_data[[2]])
    model <- build_model(input_shape = input_shape)
    
    history <- model %>% fit(
      partial_train_data, partial_train_targets,
      validation_data = list(val_data, val_targets),
      epochs = epochs, batch_size = 1, verbose = 0)
    
    mae_historie <- history$metrics$val_mae
    print(mae_historie)
    all_mae_histories <- rbind(all_mae_histories, mae_historie)
  }
  
  print(dim(all_mae_histories))
  print(all_mae_histories)
  
  average_mae_history <- data.frame(
    epoch = seq(1:ncol(all_mae_histories)),
    validation_mae = apply(all_mae_histories, 2, mean)
  )
  g.raw <- ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_line()
  print(g.raw)
  
  g.smooth <- ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_smooth()
  print(g.smooth)
}


k.fold.crossval <- function(train_data, train_targets, k = 4, epochs = 50) {
  indeces <- sample(1:nrow(train_data))
  folds <- cut(indeces, breaks = k, labels = FALSE)
  
  
  all_scores <- c()
  for (i in 1:k) {
    cat("processing fold #", i, "\n")
    
    val_indeces <- which(folds == i, arr.ind = TRUE)
    val_data <- train_data[val_indeces, ]
    val_targets <- train_targets[val_indeces]
    
    partial_train_data <- train_data[-val_indeces, ]
    partial_train_targets <- train_targets[-val_indeces]
    
    input_shape <- dim(train_data[[2]])
    model <- build_model(input_shape = input_shape)
    
    model %>% fit(
      partial_train_data, partial_train_targets,
      epochs = epochs, batch_size = 1, verbose = 0)
    
    results <- model %>% evaluate(val_data, val_targets, verbose = 0)
    all_scores <- c(all_scores, results$mean_absolute_error)
  }
  print(all_scores)
  # print(mean(all_scores))
}

# End bostom_housing_train_utils.R
