setwd("C:/Users/Khalid/Desktop/Data Science")
library(ggplot2)
library(tidyverse)
boston <- read.csv(file = 'C:/Users/Khalid/Desktop/Data Science/bostonhousing.csv')
# using sapply to get mean,median,range, var, sd, min, max und quartile
### central tendency
sapply(boston, mean, na.rm=T)
sapply(boston, median, na.rm=T)
### variability
sapply(boston, sd, na.rm=T)
sapply(boston, quantile, na.rm=T)
sapply(boston, range, na.rm=T)
## visually analyzing features
ggplot(boston, aes(x= zn)) + geom_histogram(bins = 15)
ggplot(boston, aes(x= zn)) + geom_density()
ggplot(boston, aes(x= indus)) + geom_histogram(bins = 15)
ggplot(boston, aes(x= indus)) + geom_density()
### scaling the data to make variables comparable
bostonhousing <- scale(boston)
### building heatmap to put similar observations close to each other
heatmap(bostonhousing, scale = "none")
