rm(list=ls())
library(tidyverse)

df <- read.csv("./data/logit_data.csv")
attach(df)
Y <- cbind(y)
X <- cbind(x1,x2,x3,x4,x5,x6,x7)
summary(X)
table(Y)

logit <- glm(Y ~ X,family=binomial(link='logit'))
summary(logit)

probit <- glm(Y ~ X,family=binomial(link='probit'))

summary(probit)


install.packages("AER")
library(AER)

tobit <- tobit(Y~X,left=0,right=Inf,dist="gaussian" )
summary(tobit)

tobit(Y~X,left=0,right=1,dist="gaussian" )
