library(dplyr)
library("ggvis")


file <- "https://github.com/smach/NICAR15data/raw/master/testscores.csv"

testdata <- read.csv(file, stringsAsFactors = FALSE)

ggvis(testdata, ~ pctpoor, ~ score) %>%
  layer_points(size := input_slider(10, 310, label = "Point size"),
               opacity := input_slider(0, 1, label = "Point opacity")) %>%
  layer_model_predictions(model = "lm", stroke := "red", fill := "red")

# quit()
