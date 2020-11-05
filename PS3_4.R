# Set working dir
setwd("D:/Assignment/R_Assignment03")
# Load the libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#Import Data
data     <- data.frame(
  Elevation  =  c( 180,305,381,488,549,640,762,883),
  Temperature =  c(13.3,12.2,13.3,10.0,8.3,9.4,8.3,7.2)
)
Data      <-  as_tibble(data)

# Check the class
class(Data)

# Scatter plot
plot(Temperature  ~  Elevation, data=Data)
# Linear trend
fit <- lm( Temperature  ~  Elevation,data=Data)
summary(fit)
abline(fit, col="blue")

#温度随海拔的下降速度为9.31摄氏度每千米，不等于出行条件。

