# Install missing libraries
# ----------------------------------------
# install.packages("readxl")

# Activate libraries
# ----------------------------------------

required.libraries <- c(
  "caret",
  "class",
  "data.table",
  "gclus",
  "dplyr",
  "ggvis",
  "gmodels",
  "iterators",
  "lattice",
  "lmPerm",
  "magrittr",
  "multcomp",
  "plotly",
  "readxl",
  "shiny",
  "tibble",
  "tidyverse",
  "varhandle"
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
library(ggplot2)    # Load in `ggvis`
library(gmodels)
# library(hflights)
library(Hmisc)
library(iterators)
library(lattice)
library(lmPerm)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(multcomp)
library(plotly)
library(readxl)
library(shiny)
library(tibble)
library(tidyverse)
library(varhandle)


