# ============================================ #
# Libraries
# ============================================ #
source("D:/statistics/analyses/R-Script-Analyses/dummy_toy_analyses/litter_analyses/all_libraries.R")

# ============================================ #
# Functions
# ============================================ #

# Load Dataset
# ---------------------------------------------------------------------------- #
load_datset.litter <- function() {
  data("litter") # Import data to be analyzed
  attach(litter)
  
  print(head(litter))
  print(str(litter))
  print(summary(litter))
  
  # Create data frame
  litter.df <- data.frame(litter, stringsAsFactors = FALSE)
  # First rows of data
  print(head(litter.df))
  
  # Data Cleaning and Pre-processing
  # -------------------------------------------- # 
  is.na(litter.df)
  # Filtering step:
  # litter.df.na.removed$dose <- as.numeric(as.character(litter.df.na.removed$dose))
  # litter.df.na.removed <- litter.df[!is.na(litter.df)]
  litter.df %>% drop_na()
  print(head(litter.df)) # print(head(litter.df.na.removed))
  print(str(litter.df))
  
  litter.df$dose <- as.numeric(as.character(litter.df$dose))
  print(str(litter.df))
  
  # Filter Na values by column
  # litter.df %>% drop_na(a)
  # litter.df %>% filter(a != NA)
  return(list(litter=litter, litter.df=litter.df))
}


# Plots: Boxplot Plots Section
# ---------------------------------------------------------------------------- #
boxplot.litter_dataset <- function(litter.df.scaled) {
  boxplot(litter.df.scaled[, 1], litter.df.scaled[, 2], litter.df.scaled[, 3], litter.df.scaled[, 4],
          main = "Multiple boxplots for comparison - scaled data",
          at = c(1,2,3,4),
          names = names(litter),
          las = 2,
          col = c("orange","red"),
          border = "brown",
          horizontal = TRUE,
          notch = FALSE # TRUE
  )
}


# Plots: Scatter Plots Section
# ---------------------------------------------------------------------------- #
scatter_plots.litter_dataset <- function (litter) {
  pairs(~dose+weight+gesttime+number,data = litter, main = "Scatterplot Matrix")
  # scatterplot.matrix(~dose+weight+gesttime+number, data=litter, main="Scatterplot Matrixs")
  # scatterplot(~dose+weight+gesttime+number, data=litter, main="Scatterplot Matrixs", labels=row.names(litter))
  
  # Scatterplot Matrices from the glus Package
  dta <- litter.df[c(1,2,3,4)] # get data
  dta.r <- abs(cor(dta)) # get correlations
  dta.col <- dmat.color(dta.r) # get colors
  # reorder variables so those with highest correlation
  # are closest to the diagonal
  dta.o <- order.single(dta.r)
  cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
         main="Variables Ordered and Colored by Correlation" )
}

# Plots: Pie-Chart Section
pie_plots.litter_dataset <- function (litter.df) {
  pie(litter.df$dose, main="Dose", border="brown",
      clockwise=TRUE)
  pie(litter.df$weight, main="Weight",)
  pie(litter.df$gesttime, main="Gest Time",)
  pie(litter.df$number, main="Number",)
}

# Histograms
# ---------------------------------------------------------------------------- #
hist_plots.litter_dataset <- function(litter.df, litter.df.scaled) {
  hist(litter.df$dose,
       main="Dose",
       xlab="x",
       # xlim=c(50,100),
       col="darkmagenta",
       freq=FALSE
  )
  hist(litter.df.scaled$dose,
       main="Dose - Scaled",
       xlab="x",
       # xlim=c(50,100),
       col="darkmagenta",
       freq=FALSE
  )
  hist(litter.df$weight,
       main="Weight",
       xlab="x",
       # xlim=c(50,100),
       col="darkmagenta",
       freq=FALSE
  )
  hist(litter.df.scaled$weight,
       main="Weight - Scaled",
       xlab="x",
       # xlim=c(50,100),
       col="darkmagenta",
       freq=FALSE
  )
  hist(litter.df$gesttime,
       main="Gets Time",
       xlab="x",
       # xlim=c(50,100),
       col="darkmagenta",
       freq=FALSE
  )
  hist(litter.df.scaled$gesttime,
       main="Gets Time - Scaled",
       xlab="x",
       # xlim=c(50,100),
       col="darkmagenta",
       freq=FALSE
  )
  hist(litter.df$number,
       main="Number",
       xlab="x",
       # xlim=c(50,100),
       col="darkmagenta",
       freq=FALSE
  )
  hist(litter.df.scaled$number,
       main="Number - Scaled",
       xlab="x",
       # xlim=c(50,100),
       col="darkmagenta",
       freq=FALSE
  )
}


# Stripcharts
# ---------------------------------------------------------------------------- #
stripchart_plots.litter_dataset <- function(litter.df, litter.df.scaled) {
  # stripchart(litter.df.scaled$dose)
  stripchart(litter.df.scaled$dose,
             main="Dose",
             xlab="x",
             ylab="y",
             method="jitter",
             col="orange",
             pch=1)
  # stripchart(litter.df.scaled$weight)
  stripchart(litter.df.scaled$weight,
             main="Weight",
             xlab="x",
             ylab="y",
             method="jitter",
             col="orange",
             pch=1)
  # stripchart(litter.df.scaled$gesttime)
  stripchart(litter.df.scaled$gesttime,
             main="Gets Time",
             xlab="x",
             ylab="y",
             method="jitter",
             col="orange",
             pch=1)
  # stripchart(litter.df.scaled$number)
  stripchart(litter.df.scaled$number,
             main="Number",
             xlab="x",
             ylab="y",
             method="jitter",
             col="orange",
             pch=1)
}
