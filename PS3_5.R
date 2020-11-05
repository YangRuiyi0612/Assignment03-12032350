# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Set working directory
setwd("D:/Assignment/R_Assignment03")

#Import Data 
data     <- data.frame(
  Nebula  =  c("S. Mag.","L. Mag.", "NGC 6822", "NGC 598", "NGC 221",
              "NGC 224" , "NGC 5457", "NGC 4736", "NGC 5194", "NGC 4449",
              "NGC 4214", "NGC 3031", "NGC 3627", "NGC 4626", "NGC 5236",
              "NGC 1068", "NGC 5055", "NGC 7331", "NGC 4258", "NGC 4151",
              "NGC 4382", "NGC 4472", "NGC 4486", "NGC 4649"),
  Velocity =  c(170,290,-130,-70,-185,-200,200,290,270,200,300,
                -30,650,150,500,920,450,500,500,960,500,850,800,1090),
  Distance =  c(0.032,0.034,0.214,0.263,0.275,0.275,0.450,0.500,0.500,
                0.630,0.800,0.900,0.900,0.900,0.900,1.000,1.100,
                1.100,1.400,1.700,2.000,2.000,2.000,2.000)
                
  )
Data      <-  as_tibble(data)

#5.1 Scatter plot
plot( Distance  ~ Velocity , data=Data)

#5.2 Linear trend
fit <- lm(Distance  ~ Velocity ,data=Data)
summary(fit)
abline(fit, col="blue")

#5.3 Address the first assumption with regression results
fit_assumption1 <- lm(Distance  ~ Velocity-1 ,data=Data)
abline(fit_assumption1, col="red")
summary(fit_assumption1)
# Estimate the  the age of the universe(Million years)
Universe_Age = fit_assumption1$coefficients*30.9*10^18/60/60/24/365/10^8
sprintf("宇宙的年龄是 %f 亿年",Universe_Age)

#5.4
#在report中
