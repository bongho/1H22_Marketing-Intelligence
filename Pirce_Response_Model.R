rm(list=ls())

sales <- read.csv("./data/sales_pr.csv")

out <- lm(sales~price+sales_lag+up_ratio+down_ratio, data = sales)

summary(out)
