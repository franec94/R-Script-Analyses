inspect.survey.dataframe.shape <- function (data) {
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

plot.surveys.data <- function(surveys) {
  surveys_plot <- ggplot(
    data = surveys,
    mapping = aes(x = weight, y = hindfoot_length))
  
  # surveys_plot + geom_point()
  # surveys_plot + geom_hex()
  # surveys_plot + geom_point(alpha = 0.1, color = "blue")
  # surveys_plot + geom_point(alpha = 0.1, aes(color = species_id))
  ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
    geom_boxplot(alpha = 0.1) +
    geom_jitter(alpha = 0.3, color = "tomato")
}


plot.time_series.surveys.data <- function(surveys) {
  yearly_counts <- surveys %>%
    count(year, genus)
  
  # ggplot(data = yearly_counts, aes(x = year, y = n)) + geom_line()
  ggplot(data = yearly_counts, aes(x = year, y = n, group = genus, color = genus)) +
    geom_line()
}


faceting.time_series.surveys.data <- function(surveys) {
  yearly_counts <- surveys %>%
    count(year, genus)
  ggplot(data = yearly_counts, aes(x = year, y = n)) +
    geom_line() +
    facet_wrap(facets = vars(genus))
}
