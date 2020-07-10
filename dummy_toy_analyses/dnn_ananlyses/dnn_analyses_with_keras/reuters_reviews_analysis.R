#!/usr/bin/env Rscript

# ========================================
# Setup script & Activate Libraries
# ========================================
source("reuters_reviews_utils/reuters_reviews_setup_utils.R")

# ========================================
# Define Some Functions
# ========================================

# ========================================
# Begin Script
# ========================================

dataset <- dataset_reuters(num_words = 10000)
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset

print(length(train_data))

str(train_data)
str(train_targets)

print(length(test_data))

str(test_data)
str(test_targets)

word_index <- dataset_reuters_word_index()
reverse_word_index <- names(word_index)
names(reverse_word_index) <- word_index

decoded_newswire <- sapply(train_data[[1]], function(index) {
  word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
  if (!is.null(word)) word else "?"
})
print(decoded_newswire)

# Preprocessing Data
# --------------------------------------------
x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)

one_hot_train_labels <- to_one_hot(train_targets) # to_categorical()
one_hot_test_labels <- to_one_hot(test_targets)

input_shape <- dim(x_train)[[2]]
model <- build_model(input_shape = input_shape)

val_indices <- 1:1000

x_val <- x_train[val_indices, ]
partial_x_train <- x_train[-val_indices, ]

y_val <- one_hot_train_labels[val_indices, ]
partial_y_train <- one_hot_train_labels[-val_indices, ]

# Train a Model just against overall
# trianing examples
# --------------------------------------------
model <- build_model(input_shape = input_shape)
history <- model %>%  fit(
  x_train,
  one_hot_train_labels,
  epochs = 20,
  batch_size = 512,
)
plot(history)

# Train a Model splitting training set
# into two subsets, a partial train set and
# a validation set
# --------------------------------------------
model <- build_model(input_shape = input_shape)
history <- model %>%  fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)
plot(history)

# Test Model
# --------------------------------------------
results <- model %>% evaluate(x_test)
print(results)

predictions <- model %>% predict(x_test, one_hot_test_labels)

print(dim(predictions))
print(sum(predictions[1, ]))
print(which.max(predictions[1, ]))

# Running Random Model, that is,
# a Random Classifier
# --------------------------------------------
test_labels_copy <- test_targets
test_labels_copy <- sample(test_labels_copy)
ratio_correct_answers <- length(which(test_targets == test_labels_copy)) / length(test_labels_copy)
print(ratio_correct_answers)

# quit()
