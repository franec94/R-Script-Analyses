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
# Begin Script
# ============================================ #

# Clean workspace from previous objects and plots produced
# during earlier analyses
rm(ls()); dev.off(dev.list()["RStudioGD"])

set.seed(seed = SEED) # Set seed for script repeating purposes

# ============================================ #
# Libraries
# ============================================ #

list.files(path = "D:/statistics/analyses/R-Script-Analyses/dummy_toy_analyses/litter_analyses/utils/")

source("D:/statistics/analyses/R-Script-Analyses/dummy_toy_analyses/litter_analyses/all_libraries.R")
source("D:/statistics/analyses/R-Script-Analyses/dummy_toy_analyses/litter_analyses/utils/descriptive_statistics.R")

# -------------------------------------------- #
# Data investigation:
# performing Descriptive Statistics
# -------------------------------------------- #
litter.dataset <- load_datset.litter()

litter <- litter.dataset$litter
litter.df <- litter.dataset$litter.df

litter.df.scaled <- as.data.frame(scale(litter.df))

# Plots: Scatter Plots Section
# -------------------------------------------- # 
scatter_plots.litter_dataset(litter)

# Plots: Box-Plot Section
# -------------------------------------------- # 
boxplot.litter_dataset(litter.df.scaled)

# Plots: Pie-Charts Section
# -------------------------------------------- #
pie_plots.litter_dataset(litter.df)

# Plots: Histograms Section
# -------------------------------------------- #
hist_plots.litter_dataset(litter.df, litter.df.scaled)

# Plots: Stripchart Section
# -------------------------------------------- #
stripchart_plots.litter_dataset(litter.df, litter.df.scaled)

# -------------------------------------------- #
# Linear Model n.1: Simple Scatterplot
# -------------------------------------------- #
plot(weight, gesttime, main="Scatterplot Example",
     xlab="Litter Weight ", ylab="Gest Time", pch=19)
abline(lm(gesttime~weight), col="red") # regression line (y~x)
lines(lowess(weight,gesttime), col="blue") # regression line (y~x)

# ============================================ #
# Perform models fitting
# ============================================ #

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
