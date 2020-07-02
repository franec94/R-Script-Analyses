#!/usr/bin/env Rscript

# Import Libraries
# -----------------------------------------------

# Installation Section
# install.packages("coin")

# Import Section
require(coin) # if(!require(coin)){install.packages("coin")}
library(dplyr)
library(FSA)
library(gclus)
library(multcomp)
library(lattice)
library(lmPerm)
library(tibble)
library(tidyverse)
library(varhandle)

# ----------------------------------------------- #
# Start Script
# -----------------------------------------------

# Clean workspace from previous objects and plots produced
# during earlier analyses
rm(ls())

dev.off(dev.list()["RStudioGD"])

set.seed(seed = SEED) # Set seed for script repeating purposes


# Global Variables
# -----------------------------------------------
SEED = 1234


# Input Data
# -----------------------------------------------
input = ("
 Individual  Hand     Length
 A           Left     17.5
 B           Left     18.4
 C           Left     16.2
 D           Left     14.5
 E           Left     13.5
 F           Left     18.9
 G           Left     19.5
 H           Left     21.1
 I           Left     17.8
 J           Left     16.8
 K           Left     18.4
 L           Left     17.3
 M           Left     18.9
 N           Left     16.4
 O           Left     17.5
 P           Left     15.0
 A           Right    17.6
 B           Right    18.5
 C           Right    15.9
 D           Right    14.9
 E           Right    13.7
 F           Right    18.9
 G           Right    19.5
 H           Right    21.5
 I           Right    18.5
 J           Right    17.1
 K           Right    18.9
 L           Right    17.5
 M           Right    19.5
 N           Right    16.5
 O           Right    17.4
 P           Right    15.6
")

data = read.table(textConnection(input),header=TRUE)
attach(data)

data$Individual <- as.factor(data$Individual)
data$Individual <- as.numeric(data$Individual)

data$Hand <- as.factor(data$Hand)
print(data$Hand)
data$Hand <- as.numeric(data$Hand)

# Check the data frame
print(data.head())

str(data)

summary(data)

# Remove unnecessary objects
rm(input)


# Summarize data
Summarize(Length ~ Hand,
          data=data,
          digits=3)

# ============================================ #
# Plots Section
# ============================================ #

# Plots: Boxplot
# -------------------------------------------
boxplot(Length ~ Hand,
        data=data,
        ylab="Length, cm")

data.scaled <- as.data.frame(scale(data))
boxplot(data.scaled[, 1], data.scaled[, 2], data.scaled[, 3],
        main = "Multiple boxplots for comparison - scaled data",
        at = c(1,2,3),
        names = names(data),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE # TRUE
)

# Plots: Scatter Plots
# -------------------------------------------
pairs(~Individual+Hand+Length, data = data, main = "Scatterplot Matrix")

# Scatterplot Matrices from the glus Package
dta <- data[c(1,2,3)] # get data
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

# Plots: Histograms
# --------------------------------------------
hist(data$Length,
     main="Length",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)
hist(data.scaled$Length,
     main="Length - Scaled",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)

hist(data$Hand,
     main="Hand",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)
hist(data.scaled$Hand,
     main="Hand - Scaled",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)

hist(data$Individual,
     main="Individual",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)
hist(data.scaled$Individual,
     main="Individual - Scaled",
     xlab="x",
     # xlim=c(50,100),
     col="darkmagenta",
     freq=FALSE
)

# ============================================ #
# Models Section
# ============================================ #

# Models: Fit a linear model, more precisely
#         fit an absolute linear model to the
#         data
# -------------------------------------------
Left  = data$Length[data$Hand==1] # Left  = data$Length[data$Hand=="Left"]
Right = data$Length[data$Hand==2] # Right = data$Length[data$Hand=="Right"]

plot(Left, Right,
     pch = 16,                # shape of points
     cex = 1.0,               # size of points
     xlim=c(13, 22),          # limits of x axis
     ylim=c(13, 22),          # limits of y axis
     xlab="Left hand",
     ylab="Right hand")

abline(0,1, col="blue", lwd=2) # line with intercept of 0 and slope of 1


# ============================================ #
# Tests Section
# ============================================ #

# independence_test(Length ~ Hand,
independence_test(Hand ~ Length,
                  data = data.scaled)

# symmetry_test(Length ~ Hand | Individual,
symmetry_test(Hand ~ Length | Individual,
              data = data.scaled,
              paired = TRUE, distribution = "exact", alternative = "greater")

# independence_test(Length ~ Hand,
independence_test(Hand ~ Length,
                  data = data)

# symmetry_test(Length ~ Hand | Individual,
symmetry_test(Hand ~ Length | Individual,
              data = data,
              paired = TRUE, distribution = "exact", alternative = "greater")

# quit()

# ===========================================
# References:
# =========================================== #
# - (IndependenceTest) https://www.rdocumentation.org/packages/coin/versions/1.3-1/topics/IndependenceTest
