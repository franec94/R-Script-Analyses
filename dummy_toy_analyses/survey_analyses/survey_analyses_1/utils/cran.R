libraries <- c(
  "dplyr",
  "hexbin",
  "lubridate",
  "tidyverse"
)


for (a.library in required.libraries) {
  res <- any(grepl(a.library, installed.packages()))
  if (res == F) {
    print(c("Missing", a.library))
  }
}

library("dplyr")
library("hexbin")
library("lubridate")
library("tidyverse")

# install.packages("hexbin")
# -------------------------------------------

