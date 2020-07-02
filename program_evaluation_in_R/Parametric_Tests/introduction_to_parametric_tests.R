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

# Plots: Boxplot
# -------------------------------------------
boxplot(Length ~ Hand,
        data=data,
        ylab="Length, cm")


# Models: Fit a linear model, more precisely
#         fit an absolute linear model to the
#         data
# -------------------------------------------
Left  = data$Length[data$Hand=="Left"]
Right = data$Length[data$Hand=="Right"]

plot(Left, Right,
     pch = 16,                # shape of points
     cex = 1.0,               # size of points
     xlim=c(13, 22),          # limits of x axis
     ylim=c(13, 22),          # limits of y axis
     xlab="Left hand",
     ylab="Right hand")

abline(0,1, col="blue", lwd=2) # line with intercept of 0 and slope of 1


independence_test(Length ~ Hand,
                  data = data)


symmetry_test(Length ~ Hand | Individual,
              data = data)

# quit()

# ===========================================
# References:
# =========================================== #
# - (IndependenceTest) https://www.rdocumentation.org/packages/coin/versions/1.3-1/topics/IndependenceTest