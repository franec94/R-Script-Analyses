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
# Activate Libraries
# ========================================

library("lattice")

# Built-in Dataset: 'airquality'
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #

data("airquality")

print(names(airquality))
str(airquality)
summary(airquality)

airquality$date <- with(airquality, ISOdate(1973, Month, Day))
summary(airquality)

plot(airquality$date, airquality$Temp)
plot(airquality$date, airquality$Ozone)
plot(airquality$Temp, airquality$Ozone)

# quit()

plot(Ozone~date, data=airquality, type='h')
plot(Ozone~date, data=airquality, type='l')
plot(Ozone~date, data=airquality, type='n')

plot(Ozone~Solar.R, data=airquality, type='p')
with(airquality, lines(Ozone, Solar.R, col="purple", lwd=2))

bad <- ifelse(airquality$Ozone >= 90, "orange", "forestgreen")
plot(Ozone~date, data=airquality, type='h', col=bad)
abline(h=90, lty=2, col="red")

plot(Ozone~date, data=airquality, type='b', col=bad)

coplot(
  Ozone~Solar.R | Temp * Wind, number=c(4,4),
  data=airquality,
  pch=21, col="goldenrod", bg="goldenrod"
)

# quit()
