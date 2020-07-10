#!/usr/bin/env Rscript

# ========================================
# Resources: 
# Dataset description: https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html
# ========================================

# ========================================
# Setup script & Activate Libraries
# ========================================
SEED = 1234
set.seed(seed = SEED)

setwd("./boston_housing_analyses")

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
# show.joinplot.via.seaborn(train_data, attr.1 = "CRIM", attr.2 = "ZN")

# Show Complex Plot about not yet pre-processed train set
# plot.via.seaborn.pairplot.complex(train_data)

# Show Raw Correlation Matrix about not yet pre-processed train set
# raw.plot.pairs(train_data)

# # Show Raw Boxplots about not yet pre-processed train set
raw.box.plot(train_data)

# Preprocess Dataset
# ----------------------------------------
mean.train_data <- apply(train_data, 2, mean)
std.train_data <- apply(train_data, 2, sd)

train_data <- scale(train_data, center = mean.train_data, scale = std.train_data)
test_data <- scale(test_data, center = mean.train_data, scale = std.train_data)

# Show pairplot about pre-processed train set
# plot.via.seaborn.pairplot.diag.kde(train_data)

# Show joinplot about pre-processed train set
# show.joinplot.via.seaborn(train_data, attr.1 = "CRIM", attr.2 = "ZN")

# Show Raw Correlation Matrix about yet pre-processed train set
# raw.plot.pairs(train_data)

# Show Raw Boxplots about pre-processed train set
raw.box.plot(train_data)

# quit()
