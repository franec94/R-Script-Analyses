source("./utils/plots_utils/plot_utils.R")

# Show Scatter Plots for Surveys
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
scatter.plot.surveys <- function(surveys, attr.1, attr.2, target.attr, scatter.plot.labels=NULL) {
  scatter.plot.util(surveys, attr.1, attr.2, target.attr, scatter.plot.labels)
}


multiple.scatter.plots.surveys <- function (surveys, attrs.pairs, target.attr, scatter.plot.labels=NULL, theme_cowplot_flag=F) {
  # plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)
  
  n <- length(attrs.pairs)
  labels <- 1:n
  
  p1 <- get.scatter.plot.util(
    dataset=surveys,
    attr.1= attrs.pairs$plot.1[1], 
    attr.2= attrs.pairs$plot.1[2], 
    target.attr=target.attr, 
    scatter.plot.labels=scatter.plot.labels$plot.1, 
    theme_cowplot_flag = theme_cowplot_flag
  )
  
  p2 <- get.scatter.plot.util(
    dataset=surveys,
    attr.1= attrs.pairs$plot.2[1], 
    attr.2= attrs.pairs$plot.2[2],  
    target.attr=target.attr, 
    scatter.plot.labels=scatter.plot.labels$plot.2, 
    theme_cowplot_flag = theme_cowplot_flag
  )
  
  plot_grid(p1, p2, labels = labels, label_size = 12)
}

# Show Scatter Plots for Surveys
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
density.plot.surveys <- function(surveys, attr, target.attr, scatter.plot.labels=NULL) {
  density.plot.util(surveys, attr, target.attr, scatter.plot.labels)
}


multiple.density.plots.surveys <- function (surveys, attrs, target.attr, density.plot.labels=NULL) {
  # plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)
  
  n <- length(attrs)
  labels <- 1:n
  
  p1 <- get.density.plot.util(
    dataset=surveys,
    attr=attrs[1], 
    target.attr=target.attr, 
    density.plot.labels=density.plot.labels[1], 
    theme_cowplot_flag = theme_cowplot_flag
  )
  
  p2 <- get.density.plot.util(
    dataset=surveys,
    attr=attrs[2], 
    target.attr=target.attr, 
    density.plot.labels=density.plot.labels[2], 
    theme_cowplot_flag = theme_cowplot_flag
  )
  
  plot_grid(p1, p2, labels = labels, label_size = 12)
}


# Show Some Plots
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
plot.surveys.data <- function(surveys) {
  surveys_plot <- ggplot(
    data = surveys,
    mapping = aes(x = weight, y = hindfoot_length))
  
  # surveys_plot + geom_point()
  # surveys_plot + geom_hex()
  # surveys_plot + geom_point(alpha = 0.1, color = "blue")
  # surveys_plot + geom_point(alpha = 0.1, aes(color = species_id))
  ggplot(data = surveys, mapping = aes(x = species_id, y = weight)) +
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


show.some.plots <- function(surveys) {
  
  # Plot Some Data
  # ---------------------------------------------------
  plot.surveys.data(surveys)
  
  
  # Plot Time Series Data
  # ---------------------------------------------------
  plot.time_series.surveys.data(surveys)
  
  
  # Faceting Time Series Data
  # ---------------------------------------------------
  faceting.time_series.surveys.data(surveys)
  
  
  # Complex Graphics Data
  # ---------------------------------------------------
  # pair.surveys.plot(surveys) 
  # my.surveys.save_image(surveys)
}
