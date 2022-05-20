library(tidyverse)
library(MASS)
library(pscl)
library(AER)

df <- read.csv("./data/count_data.csv")
attach(df)
df
Y <- cbind(y)
X <- cbind(x1,x2,x3,x4)

poisson <- glm(Y~X, family='poisson')
summary(poisson)

dispersiontest(poisson)

# 평규과 분산의 비율이 약 1:1.005765로 
dispersiontest(poisson,trafo = 2)
