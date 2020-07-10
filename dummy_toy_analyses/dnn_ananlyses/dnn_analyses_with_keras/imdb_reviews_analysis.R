#!/usr/bin/env Rscript

# ========================================
# Setup script
# ========================================
source("imdb_reviews_utils/imdb_reviews_setup_utils.R")


# ========================================
# Begin Script
# ========================================

# Fetch Dataset IMDB
# --------------------------------------------
dataset <-  dataset_imdb(num_words = 10000)
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

y_train <- as.numeric(train_targets)
y_test <- as.numeric(test_targets)

input_shape <- dim(x_train)[[2]]
model <- build_model(input_shape = input_shape)

val_indices <- 1:1000

x_val <- x_train[val_indices, ]
partial_x_train <- x_train[-val_indices, ]

y_val <- y_train[val_indices, ]
partial_y_train <- y_train[-val_indices, ]


# Train Models
# ----------------------------------------

# Running Random Model, that is,
# a Random Classifier
# --------------------------------------------
test_labels_copy <- test_targets
test_labels_copy <- sample(test_labels_copy)
ratio_correct_answers <- length(which(test_targets == test_labels_copy)) / length(test_labels_copy)
print(ratio_correct_answers)

# Train Default Model
# train_default_model(x_train, x_test, y_train, y_test, epochs = 5)

input_shape <- dim(x_train)[[2]]
model <- build_model(input_shape = input_shape)

history <- model %>%  fit(
  x_train,
  y_train,
  epochs = 20,
  batch_size = 512,
)
plot(history)

# Test Model (Default)
# --------------------------------------------
results <- model %>% evaluate(x_test)
print(results)

predictions <- model %>% predict(x_test, y_test)

print(dim(predictions))

# quit()
