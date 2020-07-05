# Scatter Plot Handling
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #

scatter.plot.util <- function(dataset, attr.1, attr.2, target.attr, scatter.plot.labels=NULL, theme_cowplot_flag = F) {
  
  a.scatter.plot <- get.scatter.plot.util(dataset=dataset,
                    attr.1=attr.1, 
                    attr.2=attr.2, 
                    target.attr=target.attr, 
                    scatter.plot.labels=scatter.plot.labels, 
                    theme_cowplot_flag = theme_cowplot_flag)
  
  print(a.scatter.plot)
  
}


get.scatter.plot.util <- function(dataset, attr.1, attr.2, target.attr, scatter.plot.labels=NULL, theme_cowplot_flag = F) {
  scatter.plot.theme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                              legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                              legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                              axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                              axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))
  
  # default ggplot2 theme
  if (typeof(attr.1) == 'character') {
    attr.1.local <- eval(parse(text = attr.1))
  } else {
    attr.1.local <- attr.1
  }
  if (typeof(attr.2) == 'character') {
    attr.2.local <- eval(parse(text = attr.2))
  } else {
    attr.2.local <- attr.2
  }
  
  a.scatter.plot <- ggplot(dataset, aes(attr.1.local, attr.2.local, color = target.attr))
  
  if (theme_cowplot_flag == F) {
    a.scatter.plot <- a.scatter.plot +
      geom_point()
  } else {
    a.scatter.plot <- a.scatter.plot +
      geom_point() +
      theme_cowplot(12)
  }
  
  if (is.null(scatter.plot.labels)) {
    a.scatter.plot <- a.scatter.plot + scatter.plot.theme
  } else {
    a.scatter.plot <- a.scatter.plot +
            scatter.plot.theme +
            labs(title = scatter.plot.labels[1],
                 x = scatter.plot.labels[2],
                 y = scatter.plot.labels[3],
                 fill =scatter.plot.labels[4])
  }
  return(a.scatter.plot)
}


# Density Estimate Plot Handling
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
density.plot.util <- function(dataset, attr, target.attr, density.plot.labels=NULL, theme_cowplot_flag = F) {
  # minimal horizontal grid theme
  a.density.plot <- get.scatter.plot.util(dataset=dataset,
                                          attr=attr, 
                                          target.attr=target.attr, 
                                          density.plot.labels=theme_cowplot_flag, 
                                          theme_cowplot_flag = theme_cowplot_flag)
  print(a.density.plot)
}


get.density.plot.util <- function(dataset, attr, target.attr, density.plot.labels=NULL, theme_cowplot_flag = F) {
  
  if (typeof(attr) == "character") {
    a.density.plot <- ggplot(dataset, aes(eval(parse(text = attr)), fill = target.attr)) + 
      geom_density(alpha = 0.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_hgrid(12) +
      labs(title = density.plot.labels[1],
           x = density.plot.labels[2],
           fill = density.plot.labels[3])
  } else {
    a.density.plot <- ggplot(dataset, aes(attr, fill = target.attr)) + 
      geom_density(alpha = 0.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_minimal_hgrid(12) +
      labs(title = density.plot.labels[1],
           x = density.plot.labels[2],
           fill = density.plot.labels[3])
  }

  return(a.density.plot)
}


# BoxPlot Handling
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
box.plot.util <- function(dataset, attr.1, attr.2, box.plot.labels=NULL, theme_cowplot_flag = F) {
  a.box.plot <- get.box.plot.util(dataset, attr.1, attr.2, box.plot.labels=box.plot.labels, theme_cowplot_flag = theme_cowplot_flag)
  print(a.box.plot)
}


get.box.plot.util <- function(dataset, attr.1, attr.2, box.plot.labels=NULL, theme_cowplot_flag = F) {

  print(box.plot.labels)
  
  if (typeof(attr.1) == 'character') {
    attr.1.local <- eval(parse(text = attr.1))
  } else {
    attr.1.local <- attr.1
  }
  if (typeof(attr.2) == 'character') {
    attr.2.local <- eval(parse(text = attr.2))
  } else {
    attr.2.local <- attr.2
  }
  
  if (is.null(box.plot.labels)) {
    a.box.plot <- ggplot(dataset, aes(x = attr.1.local, y = attr.2.local)) +
      geom_boxplot() +
      theme_minimal_hgrid(12)
  } else {
    a.box.plot <- ggplot(dataset, aes(x = attr.1.local, y = attr.2.local)) +
      geom_boxplot() +
      theme_minimal_hgrid(12) +
      labs(
          title = box.plot.labels[1],
          x = box.plot.labels[2],
          y = box.plot.labels[3])
  }
  return(a.box.plot)
}
