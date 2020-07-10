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
}

# End bostom_housing_train_utils.R
