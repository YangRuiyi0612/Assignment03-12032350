# Set working dir
setwd("D:/Assignment/R_Assignment03")
# Load the libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Read csv
Bone_data <- read.csv("Bone_Oxygen.csv", header=T)  #数据已经手动输入到csv文件中，读入csv文件
data      <- as_tibble(Bone_data)

data                 %>%
  group_by(BONE)     %>%                            #以骨头类型聚类
  ggplot(aes(x = BONE, y = OXYGEN, fill = BONE)) +
  geom_boxplot() +
  ylim(10,13)  +
  ylab("Oxygen isotopic composition") +
  theme_classic()

anova_one_way <- aov(OXYGEN ~ BONE, data = data)
summary(anova_one_way)
TukeyHSD(anova_one_way)
#有99.9%的概率认为这些骨头不是同一温度下形成的，即恐龙属于变温动物
