#!/usr/bin/env Rscript

# ========================================
# Setup script
# ========================================
# Remove all old data variables
if (length((ls())) != 0) {
  rm(list = ls())
}

# Close all images, panels still open
if(length(dev.list()["RStudioGD"]) != 0) {
  dev.off(dev.list()["RStudioGD"])
}

# ========================================
# Activate Packages
# ========================================
library(reticulate)
use_virtualenv("r-tensorflow")
library(keras)

# source("imdb_reviews_utils/imdb_reviews_utils.R")

# quit()
