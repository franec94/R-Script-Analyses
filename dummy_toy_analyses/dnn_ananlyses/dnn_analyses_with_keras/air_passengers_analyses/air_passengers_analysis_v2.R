#!/usr/bin/env Rscript

# ========================================
# Setup script
# ========================================

if (length((ls())) != 0) {
  rm(list = ls())
}

# Close all images panels still 
if(length(dev.list()["RStudioGD"]) != 0) {
  dev.off(dev.list()["RStudioGD"])
}

# ========================================
# Activate Packages
# ========================================

library(reticulate)
use_virtualenv("r-tensorflow")
library(keras)

# reticulate::py_discover_config() 

os <- import("os")
os$listdir(".")
py_available()

QT_QPA_PLATFORM_PLUGIN_PATH <- "C:/Users/Francesco/AppData/Local/r-miniconda/envs/r-reticulate/Library/plugins/platforms"
os$environ$putenv('QT_QPA_PLATFORM_PLUGIN_PATH', QT_QPA_PLATFORM_PLUGIN_PATH)


#importing required Python libraries/modules
sns <- import('seaborn')
plt <- import('matplotlib.pyplot')
pd <- import('pandas')

  

# ========================================
# Start Script
# ========================================

dat <- AirPassengers

str(dat)

# convert time series to data frame
dat <- data.frame(matrix(dat, ncol=frequency(dat), dimnames=dimnames(.preformat.ts(dat)) ))
dat
sns$heatmap(r_to_py(dat), fmt = "g", cmap = "viridis")
plt$show()

# quit()
