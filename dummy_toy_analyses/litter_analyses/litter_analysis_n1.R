#!/usr/bin/env Rscript

# ============================================ #
# Resources exploited:
# - (Permutation tests in R) https://www.r-bloggers.com/permutation-tests-in-r/
# - (Scatterplots) https://www.statmethods.net/graphs/
#
# Libraries exploited:
# - Packages employed(or suggested): multcomp, coin, lmPerm
# ============================================ # 

# ============================================ #
# Global Variables
# ============================================ #
SEED = 42

# ============================================ #
# Libraries
# ============================================ #

# If not installed, uncomment the following lines of code
# and let the script install missing libraries:

# install.packages("car")
# install.packages("gclus")
# install.packages("multcomp")
# install.packages("lmPerm")
# install.packages("tidyverse")
# install.packages("varhandle") 


# library(car)
library(dplyr)
library(gclus)
library(multcomp)
library(lattice)
library(lmPerm)
library(tibble)
library(tidyverse)
library(varhandle)

# ============================================ #
# Begin Script
# ============================================ #

# Clean workspace from previous objects and plots produced
# during earlier analyses
rm(ls())

dev.off(dev.list()["RStudioGD"])

set.seed(seed = SEED) # Set seed for script repeating purposes


# -------------------------------------------- #
# Data investigation:
# performing Descriptive Statistics
# -------------------------------------------- #

data(litter) # Import data to be analyzed
attach(litter)

print(head(litter))
print(str(litter))
print(summary(litter))

# Create data frame
litter.df <- data.frame(litter, stringsAsFactors = FALSE)
# First rows of data
print(head(litter.df))

# Data Cleaning and Pre-processing
# -------------------------------------------- # 
is.na(litter.df)
# Filtering step:
# litter.df.na.removed$dose <- as.numeric(as.character(litter.df.na.removed$dose))
# litter.df.na.removed <- litter.df[!is.na(litter.df)]
litter.df %>% drop_na()
print(head(litter.df)) # print(head(litter.df.na.removed))
print(str(litter.df))

litter.df$dose <- as.numeric(as.character(litter.df$dose))
print(str(litter.df))

# Filter Na values by column
# litter.df %>% drop_na(a)
# litter.df %>% filter(a != NA)

# Scatter Plots
# -------------------------------------------- # 
pairs(~dose+weight+gesttime+number,data = litter, main = "Scatterplot Matrix")
# scatterplot.matrix(~dose+weight+gesttime+number, data=litter, main="Scatterplot Matrixs")
# scatterplot(~dose+weight+gesttime+number, data=litter, main="Scatterplot Matrixs", labels=row.names(litter))

# Scatterplot Matrices from the glus Package
dta <- litter.df[c(1,2,3,4)] # get data
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

litter.df.scaled <- as.data.frame(scale(litter.df))
boxplot(litter.df.scaled[, 1], litter.df.scaled[, 2], litter.df.scaled[, 3], litter.df.scaled[, 4],
        main = "Multiple boxplots for comparison - scaled data",
        at = c(1,2,3,4),
        names = names(litter),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE # TRUE
)

# -------------------------------------------- #
# Linear Model n.1: Simple Scatterplot
# -------------------------------------------- #
plot(weight, gesttime, main="Scatterplot Example",
     xlab="Litter Weight ", ylab="Gest Time", pch=19)
abline(lm(gesttime~weight), col="red") # regression line (y~x)
lines(lowess(weight,gesttime), col="blue") # regression line (y~x)

# -------------------------------------------- #
# Perform models fitting
# -------------------------------------------- #

# Model n.1: AOV-Analysis
# -------------------------------------------- #

# Fit model n.1 with AOV statistics technique
model.n1 <- aov(weight ~ number + gesttime + dose, data=litter)
summary(model.n1)

# Plot results for model n.1
qqnorm(resid(model.n1), main="Normal Q-Q Plot")
qqline(resid(model.n1), col="red")


# Model n.2: AOVP-Analysis
# -------------------------------------------- #

# Fit model n.1 with AOVP statistics technique
model.n2 <- aovp(weight ~ number + gesttime + dose, data=litter)
summary(model.n2)

# Commeted out quit() function call
# uncomment when running this script
# from shell, as an instance.
# quit()