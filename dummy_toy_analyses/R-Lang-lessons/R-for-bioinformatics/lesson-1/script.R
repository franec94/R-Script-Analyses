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
# Salary Dataset
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# Fetch Input Data: salary.txt source file
salary.path <- "data/salary.txt"
salary <- read.table(salary.path, header=T)

# Check results: Inspect fetched data
str(salary)
# View(salary)
summary(salary)

print(table(salary$gender, salary$case))
print(table(salary$gender, salary$yrdeg))

plot(salary$year, salary$gender)

# Ozone Dataset
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# Fetch Input Data: salary.txt source file
ozone.path <- "data/ozone.csv"
ozone <- read.table(ozone.path, header=T, sep=",")

# Check results: Inspect fetched data
str(ozone)
summary(ozone)
# View(ozone)

mean(ozone$Solar.R)
median(ozone$Solar.R)
var(ozone$Solar.R)
sd(ozone$Solar.R)

mean(ozone$Wind)
median(ozone$Wind)
var(ozone$Wind)
sd(ozone$Wind)

# Built-in Dataset: 'cars'
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
data(cars)

plot(dist~speed, data=cars)
with(cars, lines(lowless(speed, dist), col="tomato", lwd=2))

plot(dist~speed, data=cars, log="xy")
with(cars, lines(lowless(speed, dist), col="tomato", lwd=2))
with(cars, lines(supsmu(speed, dist), col="purple", lwd=2))

legend("bottomright", legend = c("lowless", "supersmoother"), bty="n",
       lwd=2, col=c("tomato", "purple"))
# Links
# -----
# (Permutation Tet: Tutorial):
# https://thomasleeper.com/Rcourse/Tutorials/permutationtests.html
