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

#importing required Python libraries/modules
sns <- import('seaborn')
plt <- import('matplotlib.pyplot')
pd <- import('pandas')

#using R's inbuilt AirPassengers dataset
df <- datasets::AirPassengers
#converting Time-Series object into an R Dataframe 
#Thx: https://stackoverflow.com/questions/5331901/transforming-a-time-series-into-a-data-frame-and-back
df1 <- data.frame(tapply(df, list(year = floor(time(df)), month = month.abb[cycle(df)]), c))
df1 <- df1[month.abb]
#building a heatmap using seaborn 
#please note the function r_to_py() that converts R object into a python 
sns$heatmap(r_to_py(df1), fmt="g", cmap ='viridis')
#display the plot
plt$show()

# quit()
