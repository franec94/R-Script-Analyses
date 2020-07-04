# Install missing libraries
# ----------------------------------------
# install.packages("readxl")

# Activate libraries
# ----------------------------------------

required.libraries <- c(
  "caret",
  "class",
  "data.table",
  "dplyr",
  "ggvis",
  "gmodels",
  "iterators",
  "magrittr",
  "readxl",
  "tidyverse"
)

for (a.library in required.libraries) {
  res <- any(grepl(a.library, installed.packages()))
  if (res == F) {
    print(c("Missing", a.library))
  }
}


library(caret)
library(class)
library(data.table)
library(dplyr)    # alternatively, this also loads %>%
library(gclus)
library(ggvis)    # Load in `ggvis`
library(gmodels)
library(iterators)
library(lattice)
library(lmPerm)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(multcomp)
library(tibble)
library(readxl)
library(tidyverse)
library(varhandle)


