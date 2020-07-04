library(ggvis)

# The first few rows of mtcars
head(mtcars)
attach(mtcars)

qvis(mtcars, ~wt, ~mpg)
# ggvis(mtcars, props(x = ~wt, y = ~mpg)) + mark_point()