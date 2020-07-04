#!/usr/bin/env Rscript

source("utils/cran.R")
source("utils/funcs.R")

# Load Data from Survey
# surveys <- read.csv("data_raw/portal_data_joined.csv")
surveys <- read.csv("data_raw/portal_data_joined.csv", stringsAsFactors = TRUE)

levels(surveys$sex)[1] <- "undetermined"


# Inspect Data
# ---------------------------------------------------
print(head(surveys))

# Try also
# View(surveys)

str(surveys)


# Inspect Data via Plots
# ---------------------------------------------------

# Bar plot of the number of females and males captured during the experiment:
plot(as.factor(surveys$sex))


# Inspect Date-time Data
# ---------------------------------------------------
surveys$date <- ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
str(surveys) # notice the new column, with 'date' as the class
summary(surveys$date)


# Inspect Missing Data
# ---------------------------------------------------
missing_dates <- surveys[is.na(surveys$date), c("year", "month", "day")]

print(head(missing_dates))


# Filtering Data
# ---------------------------------------------------
# filter.surveys.data(surveys)


# Mutate Data
# ---------------------------------------------------
# mutate.surveys.data(surveys)


# Summarize Data
# ---------------------------------------------------
# summarize.surveys.data(surveys)


# Plot Data
# ---------------------------------------------------
# plot.surveys.data(surveys)


# Plot Time Series Data
# ---------------------------------------------------
# plot.time_series.surveys.data(surveys)


# Faceting Time Series Data
# ---------------------------------------------------
# faceting.time_series.surveys.data(surveys)


pair.surveys.plot(surveys) 
my.surveys.save_image(surveys)


# quit()
