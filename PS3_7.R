
# Set working dir
setwd("D:/Assignment/R_Assignment03")

# Load the libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#7.1
#�ֱ�ȡ��������ĳ�غ�������ĳ�ص�ǳ�����ˮˮ������ͬ��ʵ�������£����ˮ���е�CaCo3�������Դ���t���顣
LONGGANG <- c(20.99,20.41,20.10,20.00,20.91,22.60,20.99,20.42,20.90,22.99,23.12,20.89)
LONGHUA  <- c(20.78,21.36,20.10,23.06,21.50,20.81,22.42,20.48,22.90,20.13,20.26,21.86)
t.test(LONGGANG,LONGHUA,paired=T)

#7.2
#�����о���ˮ��������Ӱ��̶���Ҫȡ����ˮ�������������Լ�ˮ�����״����Χ���εȡ�
#һ��˵����ˮ�����������Խ�󣬶���Χ���������Ӱ��ҲԽ��
#ˮ�������Χ�����Ӱ�죬�漰���¡��硢�¶ȵ�����Ҫ�غͽ�ˮ��������������
#����ͨ�������������ɻ���Storm Lake��Զ��ɽ�ص���Sioux Rapids4E�Ľ��������ݣ��ó�ˮ��Խ������Ƿ���Ӱ�죬Ӱ����

# Load data
STORM_LAKE_P_value    <-  read.csv("STORM LAKE.csv",header=T)
STORM_LAKE_P_value[is.na(STORM_LAKE_P_value)]      <- 0
SIOUX_RAPIDS_P_value  <- read.csv("SIOUX RAPIDS 4 E,.csv",header=T)
SIOUX_RAPIDS_P_value[is.na(SIOUX_RAPIDS_P_value)]  <- 0
DATA                  <- as_tibble(bind_rows(STORM_LAKE_P_value,SIOUX_RAPIDS_P_value))


# Check the class
class(DATA)
# Check the variables
head(DATA)
tail(DATA)
# plot the time series
DATA %>%
  ggplot( aes(x=as.Date(DATE), y=PRCP, color=NAME) ) + 
  geom_point() + 
  geom_line() +
  labs(title="PRCP PER city from 2005 to 2020", x="DATE", y="PRCP per city") +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20)) + 
  scale_color_discrete(name="NAME") +
  facet_wrap( ~ NAME,nrow=2)

bartlett.test(PRCP ~ NAME, data = DATA)
# ANOVA
anova_one_way <- aov(PRCP ~ NAME, data = DATA)
summary(anova_one_way)

#7.3  �о�CAS city �Ľ������ͺӵ������Ĺ�ϵ�����м򵥵����Իع�
# Load data
Discharge_data      <-  read.csv("discharge.csv",header=T)
Discharge_Data      <-  as_tibble(Discharge_data)

# Scatter plot
plot( Dischrge_ft  ~  PRCP_inch_SAC_CITY, data=Discharge_Data)

# Linear trend
fit0           <- lm( Dischrge_ft  ~  1,data=Discharge_Data)
fit            <- lm( Dischrge_ft  ~  PRCP_inch_SAC_CITY,data=Discharge_Data)
model_step_f   <- step(fit,scope=list(lower=fit0,upper=fit),direction='forward')
summary(model_step_f)
abline(model_step_f, col="blue")

