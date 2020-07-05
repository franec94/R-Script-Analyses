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
# Import Libraries
# ========================================
getwd()

source("utils/cran.R")
source("utils/funcs.R")
source("utils/plots_funcs.R")


# ========================================
# Data Investigation Step 
# ========================================

# Load Data from Survey
# surveys <- read.csv("data_raw/portal_data_joined.csv")
surveys <- read.csv("data_raw/portal_data_joined.csv", stringsAsFactors = TRUE)
levels(surveys$sex)[1] <- "undetermined"

ind <- sample(2, nrow(surveys), replace=TRUE, prob=c(0.67, 0.33))

# Inspect Data
# ---------------------------------------------------
print(head(surveys))

# Try also
# View(surveys)

str(surveys)


# Inspect Data via Plots
# ---------------------------------------------------

# Bar plot of the number of females and males captured during the experiment:
# plot(as.factor(surveys$sex))


# Inspect Date-time Data
# ---------------------------------------------------
surveys$date <- ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
str(surveys) # notice the new column, with 'date' as the class
summary(surveys$date)

# Attach before starting further computations
# ---------------------------------------------------
attach(surveys)


# Inspect Missing Data
# ---------------------------------------------------
missing_dates <- surveys[is.na(surveys$date), c("year", "month", "day")]

print(head(missing_dates))


# Some Data Manipulation
# ---------------------------------------------------
inspect.surveys.data.manipulation(surveys)


# Plot Data
# ---------------------------------------------------
scatter.plot.labels <- c('Genus vs Species', 'genus', 'species', 'sex')
# scatter.plot.surveys(surveys[ind, ], genus, species, sex, scatter.plot.labels)

attrs.pairs <- list(
  c("genus", "species"),
  c("genus", "weight")
)
names(attrs.pairs) <- c("plot.1", "plot.2")
scatter.plot.labels <- list(
  c('Genus vs Species', 'genus', 'species', 'sex'),
  c('Genus vs Weight', 'genus', 'weight', 'sex')
)
names(scatter.plot.labels) <- c("plot.1", "plot.2")
multiple.scatter.plots.surveys(surveys, attrs.pairs, target.attr=sex, scatter.plot.labels=scatter.plot.labels )

density.plot.labels <- c('Density Estimate Weight Attribute', 'genus', 'sex')
# density.plot.surveys(surveys[ind, ], weight, sex, density.plot.labels)

attrs <- c("weight", "genus")
density.plot.labels <- c(
  c('Density Estimate', 'weight', 'sex'),
  c('Density Estimate', 'genus', 'sex')
)
# multiple.density.plots.surveys(surveys, attrs, target.attr=sex, density.plot.labels=density.plot.labels)

# ========================================
# Train Step
# ========================================

# quit()
