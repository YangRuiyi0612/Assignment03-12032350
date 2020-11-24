# Set working dir
setwd("D:/Assignment/R_Assignment03")
# Load the libraries
library(tidyr)
library(dplyr)
library(ggplot2)
#Import Data
Unseeded  <- c(1202.6,830.1,372.4,345.5,321.2,244.3,163.0,147.8,95.0,87.0,81.2,68.5,47.3,41.1,36.6,29.0,28.6,26.3,26.0,24.4,21.4,17.3,11.5,4.9,4.9,1.0) 
seeded    <- c(2745.6,1697.1,1656.4,978.0,703.4,489.1,430.0,334.1,302.8,274.7,274.7,255.0,242.5,200.7,198.6,129.6,119.0,118.3,115.3,92.4,40.6,32.7,31.4,17.5,7.7, 4.1)
data1     <- data.frame(
  Type  = c( "Unseeded","Unseeded","Unseeded","Unseeded","Unseeded",
            "Unseeded","Unseeded","Unseeded","Unseeded","Unseeded",
            "Unseeded","Unseeded","Unseeded","Unseeded","Unseeded",
            "Unseeded","Unseeded","Unseeded","Unseeded","Unseeded",
            "Unseeded","Unseeded","Unseeded","Unseeded","Unseeded","Unseeded"),
  Rainfall= c(1202.6,830.1,372.4,345.5,321.2,244.3,163.0,147.8,95.0,87.0,81.2,68.5,47.3,41.1,36.6,29.0,28.6,26.3,26.0,24.4,21.4,17.3,11.5,4.9,4.9,1.0) 
)
data2     <- data.frame(
  Type  = c( "seeded","seeded","seeded","seeded","seeded",
             "seeded","seeded","seeded","seeded","seeded",
             "seeded","seeded","seeded","seeded","seeded",
             "seeded","seeded","seeded","seeded","seeded",
             "seeded","seeded","seeded","seeded","seeded","seeded"),
  Rainfall=  c(2745.6,1697.1,1656.4,978.0,703.4,489.1,430.0,334.1,302.8,274.7,274.7,255.0,242.5,200.7,198.6,129.6,119.0,118.3,115.3,92.4,40.6,32.7,31.4,17.5,7.7, 4.1)
)
#按行合并数据，以便将两个箱线图画在一张图里
Data      <-  as_tibble(bind_rows(data1,data2))
Unseeded_value  <- Data    %>%          #提取未播种的数据
  filter(Type=="Unseeded") %>%
  select( Rainfall )
seeded_value    <- Data    %>%          #提取播种的数据
  filter(Type=="seeded")   %>%
  select(Rainfall)

# Check the class
class(Data)

#1.1  Box plots 
summary(Data)
Data %>% 
  ggplot(aes(x = Type, y = Rainfall, fill = Type)) +
  geom_boxplot() +
  ylim(0,3000)+
  theme_classic()

#1.2  
t.test(Unseeded,seeded)

# MingYANG recommended:
# these code seems to be too long,try this:
Unseeded <- c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 147.8, 95.0, 87.0, 81.2, 68.5, 47.3, 41.1, 36.6, 29.0, 28.6, 26.3, 26.0, 24.4, 21.4, 17.3, 11.5, 4.9, 4.9, 1.0)
Seeded <- c(2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 334.1, 302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 198.6, 129.6, 119.0, 118.3, 115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1)

rainfall <- cbind(Unseeded,Seeded)
data <- data.frame(rainfall)
boxplot(rainfall,width=c(1,2),col=c(2,7),border=c("purple","black"))

t.test(Unseeded,Seeded)
# the end
