# source("reuters_reviews_build_utils.R")

# Train Default Model
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
train_default_model <- function(train_data, test_data, train_targets, test_targets) {
  # Train Model (Default)
  # --------------------------------------------
  input_shape <- dim(train_data)[[2]]
  model <- build_model(input_shape = input_shape)
  
  history <- model %>%  fit(
    train_data,
    train_targets,
    epochs = 20,
    batch_size = 512,
  )
  plot(history)
  
  # Test Model (Default)
  # --------------------------------------------
  results <- model %>% evaluate(x_test)
  print(results)

  predictions <- model %>% predict(x_test, one_hot_test_labels)

  print(dim(predictions))
  print(sum(predictions[1, ]))
  print(which.max(predictions[1, ]))

  # Train Model (Dropping-out)
  # --------------------------------------------
  input_shape <- dim(train_data)[[2]]
  model <- build_model_with_l2_reg(input_shape = input_shape)
  
  history <- model %>%  fit(
    train_data,
    train_targets,
    epochs = 20,
    batch_size = 512,
  )
  plot(history)
  
  # Test Model (Dropping-out)
  # --------------------------------------------
  results <- model %>% evaluate(x_test)
  print(results)

  predictions <- model %>% predict(x_test, one_hot_test_labels)

  print(dim(predictions))
  print(sum(predictions[1, ]))
  print(which.max(predictions[1, ]))

  # Train Model (L2-Norm)
  # --------------------------------------------
  input_shape <- dim(train_data)[[2]]
  model <- build_model_with_l2_reg(input_shape = input_shape)
  
  history <- model %>%  fit(
    train_data,
    train_targets,
    epochs = 20,
    batch_size = 512,
  )
  plot(history)
  
  # Test Model (L2-Norm)
  # --------------------------------------------
  results <- model %>% evaluate(x_test)
  print(results)

  predictions <- model %>% predict(x_test, one_hot_test_labels)

  print(dim(predictions))
  print(sum(predictions[1, ]))
  print(which.max(predictions[1, ]))
}