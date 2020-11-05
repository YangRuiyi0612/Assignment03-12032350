# Set working dir
setwd("D:/Assignment/R_Assignment03")
# Load the libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Load data
library(MASS)
library(leaps)
data(cpus)

# Check col names
head(cpus)
nrow(cpus)
#split the dataset randomly into two subsets
ind <- sample(nrow(cpus), replace=FALSE,0.8*nrow(cpus))   #训练集80%数据点，测试集20%数据点
trainData <- cpus[ind,]
testData  <- cpus[-ind,]
nrow(trainData)
nrow(testData)

# 6.1 
subset_result <- regsubsets(perf ~ syct+ mmin + mmax + cach +
                              chmin + chmax, data=trainData, nbest=2, nvmax = 6)
plot(subset_result, scale="bic")
summary(subset_result)
# 6.2
# Build a linear model
nullmodel=lm(perf ~ 1, data=trainData)
trainmodel  <- lm(perf ~ syct+ mmin + mmax + cach +
               chmin + chmax, data=trainData)

model_step_b <- step(trainmodel,direction='backward')
model_step_f <- step(nullmodel, scope=list(lower=nullmodel, upper=trainmodel), direction='forward')
model_step_s <- step(nullmodel, scope=list(lower=nullmodel, upper=trainmodel), direction='both')

# Apply the model model_step_s to the test subset
perf_predict <- predict(model_step_s,testData)

# Compare predicted values with actual values 
plot(testData$perf, perf_predict)
fit  <- lm(  testData$perf ~ perf_predict ,data=Discharge_Data)
abline(fit, col="blue")
summary(fit)

# Correlation coefficients
cor(testData$perf, perf_predict)

# Mean predicted value
mean(perf_predict)

# Mean actual value
mean(testData$perf)

# Relative mean bias
(mean(perf_predict) - mean(testData$perf))/
  mean(testData$perf)*100




