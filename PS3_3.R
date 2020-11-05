# Set working dir
setwd("D:/Assignment/R_Assignment03")
# Load the libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#Import Data
#定义pregnant:怀孕状况：因子水平：1和2(1为怀孕)
#定义vegetarians：是否素食者；因子水平：1和2(1为素食者)
a   <-  data.frame(pregnant   = c(1,1,1,1,1,1),
                  vegetarians = c(2,2,2,2,2,2),
                  zinc_levels = c(185,189,187,181,150,176))
b   <-  data.frame(pregnant   = c(1,1,1,1,1,1,1,1,1,1,1,1),
                  vegetarians = c(1,1,1,1,1,1,1,1,1,1,1,1),
                  zinc_levels = c(171,174,202,171,207,125,189,179,163,174,184,186))
c   <-  data.frame(pregnant   = c(2,2,2,2,2),
                  vegetarians = c(2,2,2,2,2),
                  zinc_levels = c(210,139,172,198,177))
data<-  bind_rows(a,b,c)

# Change to factor type
Data <- as_tibble(data) %>% 
  mutate(pregnant    = factor(pregnant, ordered = TRUE)) %>% 
  mutate(vegetarians = factor(vegetarians, ordered = TRUE)) 

Data %>% 
  ggplot(aes(x =vegetarians , y =zinc_levels , fill = vegetarians,color="red")) +
  geom_boxplot() +
  theme_classic()
# 3
aov1 <- aov( zinc_levels~vegetarians, data=Data)
summary(aov1)
aov2 <- aov( zinc_levels~vegetarians + pregnant, data=Data)
summary(aov2)