#!/usr/bin/env Rscript

# ============================================ #
# Resources exploited:
# --------------------
# - (Permutation tests in R) https://www.r-bloggers.com/permutation-tests-in-r/
# - (Scatterplots) https://www.statmethods.net/graphs/
#
# Libraries exploited:
# --------------------
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

# Close all images panels still opened
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

# Plots: Scatter Plots Section
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

# Plots: Box-Plot Section
# -------------------------------------------- # 
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

# Plots: Pie-Charts Section
# -------------------------------------------- #
pie(litter.df$dose, main="Dose", border="brown",
    clockwise=TRUE)
pie(litter.df$weight, main="Weight",)
pie(litter.df$gesttime, main="Gest Time",)
pie(litter.df$number, main="Number",)

# Plots: Histograms Section
# -------------------------------------------- #
hist(litter.df$dose,
     main="Dose",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)
hist(litter.df.scaled$dose,
     main="Dose - Scaled",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)
hist(litter.df$weight,
     main="Weight",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)
hist(litter.df.scaled$weight,
     main="Weight - Scaled",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)
hist(litter.df$gesttime,
     main="Gets Time",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)
hist(litter.df.scaled$gesttime,
     main="Gets Time - Scaled",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)
hist(litter.df$number,
     main="Number",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)
hist(litter.df.scaled$number,
     main="Number - Scaled",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)

# Plots: Stripchart Section
# -------------------------------------------- #
# stripchart(litter.df.scaled$dose)
stripchart(litter.df.scaled$dose,
           main="Dose",
           xlab="x",
           ylab="y",
           method="jitter",
           col="orange",
           pch=1)
# stripchart(litter.df.scaled$weight)
stripchart(litter.df.scaled$weight,
           main="Weight",
           xlab="x",
           ylab="y",
           method="jitter",
           col="orange",
           pch=1)
# stripchart(litter.df.scaled$gesttime)
stripchart(litter.df.scaled$gesttime,
           main="Gets Time",
           xlab="x",
           ylab="y",
           method="jitter",
           col="orange",
           pch=1)
# stripchart(litter.df.scaled$number)
stripchart(litter.df.scaled$number,
           main="Number",
           xlab="x",
           ylab="y",
           method="jitter",
           col="orange",
           pch=1)

# Commeted out quit() function call
# uncomment when running this script
# from shell, as an instance.
# quit()
 