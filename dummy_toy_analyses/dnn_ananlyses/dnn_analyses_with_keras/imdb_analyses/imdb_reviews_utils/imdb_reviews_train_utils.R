# source("imdb_reviews_build_utils.R")

# Train Default Model
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
train_default_model <- function(x_train, x_test, y_train, y_test, epochs = 20) {
  
  # Train Model (Default)
  # --------------------------------------------
  input_shape <- dim(x_train)[[2]]
  model <- build_model(input_shape = input_shape)
  
  history <- model %>%  fit(
    x_train,
    y_train,
    epochs = epochs,
    batch_size = 512,
  )
  plot(history)
  
  # Test Model (Default)
  # --------------------------------------------
  results <- model %>% evaluate(x_test)
  print(results)
  
  predictions <- model %>% predict(x_test, y_test)
  
  print(dim(predictions))
  
  # Train Model (Dropping-Out)
  # --------------------------------------------
  input_shape <- dim(x_train)[[2]]
  model <- build_model_with_l2_reg(input_shape = input_shape)
  
  history <- model %>%  fit(
    x_train,
    y_train,
    epochs = epochs,
    batch_size = 512,
  )
  plot(history)
  
  # Test Model (Dropping-out)
  # --------------------------------------------
  results <- model %>% evaluate(x_test)
  print(results)
  
  predictions <- model %>% predict(x_test, y_test)
  
  print(dim(predictions))
  
  # Train Model (L2-norm)
  # --------------------------------------------
  input_shape <- dim(x_train)[[2]]
  model <- build_model_with_l2_reg(input_shape = input_shape)
  
  history <- model %>%  fit(
    x_train,
    y_train,
    epochs = epochs,
    batch_size = 512,
  )
  plot(history)
  
  # Test Model (L2-norm)
  # --------------------------------------------
  results <- model %>% evaluate(x_test)
  print(results)
  
  predictions <- model %>% predict(x_test, y_test)
}

# End imdb_reviews_train_utils
