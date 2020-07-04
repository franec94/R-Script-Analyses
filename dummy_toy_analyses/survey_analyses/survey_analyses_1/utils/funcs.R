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
  
  ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
    geom_line() +
    facet_wrap(vars(genus)) +
    labs(title = "Observed genera through time",
         x = "Year of observation",
         y = "Number of individuals") +
    theme_bw() +
    theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(colour = "grey20", size = 12),
          strip.text = element_text(face = "italic"),
          text = element_text(size = 16))
  
  yearly_sex_counts <- surveys %>%
    count(year, genus, sex)
  ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
    geom_line() +
    facet_grid(rows =  vars(sex), cols = vars(genus))
  
  # One column, facet by rows
  ggplot(data = yearly_sex_counts, 
         mapping = aes(x = year, y = n, color = sex)) +
    geom_line() +
    facet_grid(rows = vars(genus))
  
  # One row, facet by column
  ggplot(data = yearly_sex_counts, 
         mapping = aes(x = year, y = n, color = sex)) +
    geom_line() +
    facet_grid(cols = vars(genus))
}


boxplot.surveys.complex.plot <- function(surveys) {
  grey_theme <- theme(axis.text.x = element_text(colour="grey20", size = 12, 
                                                 angle = 90, hjust = 0.5, 
                                                 vjust = 0.5),
                      axis.text.y = element_text(colour = "grey20", size = 12),
                      text=element_text(size = 16))
  
  ggplot(surveys, aes(x = species_id, y = hindfoot_length)) +
    geom_boxplot() +
    grey_theme
}


pair.surveys.plot <- function (surveys) {
  yearly_counts <- surveys %>%
    count(year, genus)
  
  spp_weight_boxplot <- ggplot(data = surveys, 
                               aes(x = species_id, y = weight)) +
    geom_boxplot() +
    labs(x = "Species", 
         y = expression(log[10](Weight))) +
    scale_y_log10() + 
    labs()
  
  spp_count_plot <- ggplot(data = yearly_counts, 
                           aes(x = year, y = n, color = genus)) +
    geom_line() + 
    labs(x = "Year", y = "Abundance")
  
  grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))
  
  ## This also works for grid.arrange() plots
  combo_plot <- grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, 
                             widths = c(4, 6))
  ggsave("combo_plot_abun_weight.png", combo_plot, width = 10, dpi = 300)
}


my.surveys.save_image <- function(surveys_complete) {
  yearly_sex_counts <- surveys %>%
    count(year, genus, sex)
  my_plot <- ggplot(data = yearly_sex_counts, 
                    aes(x = year, y = n, color = sex)) +
    geom_line() +
    facet_wrap(vars(genus)) +
    labs(title = "Observed genera through time",
         x = "Year of observation",
         y = "Number of individuals") +
    theme_bw() +
    theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90,
                                     hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(colour = "grey20", size = 12),
          text = element_text(size = 16))
  
  ggsave("name_of_file.png", my_plot, width = 15, height = 10)
}
