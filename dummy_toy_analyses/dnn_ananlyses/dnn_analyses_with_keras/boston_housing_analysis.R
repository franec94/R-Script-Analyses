#!/usr/bin/env Rscript

# ========================================
# Setup script & Activate Libraries
# ========================================
source("boston_housing_utils/boston_housing_setup_utils.R")

# ========================================
# Define Some Functions
# ========================================

# ========================================
# Begin Script
# ========================================

# Load Dataset 
# and check whether everything is ok
# ----------------------------------------
dataset <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset

str(train_data)
str(train_targets)

str(test_data)
str(test_targets)

# print(typeof(train_data))
# print(dim(train_data))

# Show pairplot about not yet pre-processed train set
# plot.via.seaborn.pairplot.diag.kde(train_data)

# Show joinplot about not yet pre-processed train set
show.joinplot.via.seaborn(train_data, attr.1 = "CRIM", attr.2 = "ZN")

# Preprocess Dataset
# ----------------------------------------
mean.train_data <- apply(train_data, 2, mean)
std.train_data <- apply(train_data, 2, sd)

train_data <- scale(train_data, center = mean.train_data, scale = std.train_data)
test_data <- scale(test_data, center = mean.train_data, scale = std.train_data)

# Show pairplot about pre-processed train set
plot.via.seaborn.pairplot.diag.kde(train_data)

# Show joinplot about not yet processed train set
# show.joinplot.via.seaborn(train_data, attr.1 = "CRIM", attr.2 = "ZN")

# Train Models
# ----------------------------------------

# Train Default Model
train_default_model(train_data, test_data, train_targets, test_targets)

# quit()
