required.libraries <- c(
  "cowplot",
  "dplyr",
  "gridExtra",
  "hexbin",
  "lubridate",
  "tidyverse"
)


for (a.library in required.libraries) {
  res <- any(grepl(a.library, installed.packages()))
  if (res == F) {
    print(c("Missing", a.library))
    # install.packages(a.library)
  }
}

library(cowplot)
library("dplyr")
library(gridExtra)
library("hexbin")
library("lubridate")
library("tidyverse")

# install.packages("hexbin")
# -------------------------------------------

