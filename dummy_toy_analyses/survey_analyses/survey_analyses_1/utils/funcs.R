# Dataframe Inspection
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
inspect.surveys.dataframe.shape <- function (data) {
  inspect.dataframe.shape(data)
}


inspect.dataframe.shape <- function (data) {
  
  # Size Section
  print(c("Rows x Columns:", dim(data)))
  print(c("Rows:", nrow(data)))
  print(c("Columns:", ncol(data)))
  
  # Content Section
  print("Head:")
  print(head(data))
  print("Tail:")
  print(tail(data))
  
  # Names Section
  print("Column Names:")
  print(names(data))
  print("Row Names:")
  print(rownames(data))
  
  # Summary Section
  str(data)
  summary(data)
}


# Data Manipulation
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
filter.surveys.data <- function(surveys) {
  surveys_sml <- surveys %>%
    filter(weight < 5) %>%
    select(species_id, sex, weight)
  str(surveys_sml)
  print(head(surveys_sml))
}
 
 
mutate.surveys.data <- function(surveys) {
  surveys %>%
    mutate(weight_kg = weight / 1000) %>%
    head()
  
  surveys %>%
    mutate(weight_kg = weight / 1000,
           weight_lb = weight_kg * 2.2) %>%
    head()
  
  surveys %>%
    filter(!is.na(weight)) %>%
    mutate(weight_kg = weight / 1000) %>%
    head()
  
  surveys %>%
    filter(!is.na(weight)) %>%
    mutate(weight_kg = weight / 1000,
           weight_lb = weight_kg * 2.2) %>%
    head()
}


summarize.surveys.data <- function(surveys) {
  surveys %>%
    group_by(sex) %>%
    summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
    head()
  
  surveys %>%
    group_by(sex) %>%
    summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
    tail()
  
  surveys %>%
    filter(!is.na(weight)) %>%
    group_by(sex, species_id) %>%
    summarize(mean_weight = mean(weight)) %>%
    head()
}


inspect.surveys.data.manipulation <- function(surveys) {
  # Filtering Data
  # ---------------------------------------------------
  filter.surveys.data(surveys)
  
  
  # Mutate Data
  # ---------------------------------------------------
  mutate.surveys.data(surveys)
  
  
  # Summarize Data
  # ---------------------------------------------------
  summarize.surveys.data(surveys)
}
