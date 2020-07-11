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
# library(mise); mise(vars = FALSE, figs = FALSE)
# shell("cls")

library(dplyr)
library(gclus)
library(ggplot2)
library(multcomp)
library(lattice)
library(lmPerm)
library(tibble)
library(tidyverse)
library(varhandle)

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


source("breast_cancer_wisconsin_utils/breast_cancer_wisconsin_general_purpose_util.R")

# source("breast_cancer_wisconsin_utils/breast_cancer_wisconsin_build_utils.R")
# source("boston_housing_utils/breast_cancer_wisconsin_train_utils.R")
# source("breast_cancer_wisconsin_utils/breast_cancer_wisconsin_plot_utils.R")

# End bostom_housing_setup_utils.R
