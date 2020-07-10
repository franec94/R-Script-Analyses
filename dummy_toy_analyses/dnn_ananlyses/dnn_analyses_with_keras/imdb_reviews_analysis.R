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

one_hot_train_labels <- to_one_hot(train_targets) # to_categorical()
one_hot_test_labels <- to_one_hot(test_targets)

input_shape <- dim(x_train)[[2]]
model <- build_model(input_shape = input_shape)

val_indices <- 1:1000

x_val <- x_train[val_indices, ]
partial_x_train <- x_train[-val_indices, ]

y_val <- one_hot_train_labels[val_indices, ]
partial_y_train <- one_hot_train_labels[-val_indices, ]


# Train Models
# ----------------------------------------

# Train Default Model
train_default_model(
  x_train, x_test,
  one_hot_train_labels, one_hot_test_labels,
  epochs = 5)

# quit()
