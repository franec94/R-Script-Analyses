# source("imdb_reviews_build_utils.R")

# Train Default Model
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
train_default_model <- function(x_train, x_test, one_hot_train_labels, one_hot_test_labels, epochs = 20) {
  
  # Train Model (Default)
  # --------------------------------------------
  input_shape <- dim(x_train)[[2]]
  model <- build_model(input_shape = input_shape)
  
  history <- model %>%  fit(
    x_train,
    one_hot_train_labels,
    epochs = epochs,
    batch_size = 512,
  )
  plot(history)
  
  # Test Model (Default)
  # --------------------------------------------
  results <- model %>% evaluate(x_test)
  print(results)
  
  predictions <- model %>% predict(x_test, one_hot_test_labels)
  
  print(dim(predictions))
  
  # Train Model (Dropping-Out)
  # --------------------------------------------
  input_shape <- dim(x_train)[[2]]
  model <- build_model_with_l2_reg(input_shape = input_shape)
  
  history <- model %>%  fit(
    x_train,
    one_hot_train_labels,
    epochs = epochs,
    batch_size = 512,
  )
  plot(history)
  
  # Test Model (Dropping-out)
  # --------------------------------------------
  results <- model %>% evaluate(x_test)
  print(results)
  
  predictions <- model %>% predict(x_test, one_hot_test_labels)
  
  print(dim(predictions))
  
  # Train Model (L2-norm)
  # --------------------------------------------
  input_shape <- dim(x_train)[[2]]
  model <- build_model_with_l2_reg(input_shape = input_shape)
  
  history <- model %>%  fit(
    x_train,
    one_hot_train_labels,
    epochs = epochs,
    batch_size = 512,
  )
  plot(history)
  
  # Test Model (L2-norm)
  # --------------------------------------------
  results <- model %>% evaluate(x_test)
  print(results)
  
  predictions <- model %>% predict(x_test, one_hot_test_labels)
}

# End imdb_reviews_train_utils
