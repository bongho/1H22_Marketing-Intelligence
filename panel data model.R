install.packages('plm')

library(plm)
library(tidyverse)
p_data <- read.csv("./data/paneldata.csv")
glimpse(p_data)
pdata <- pdata.frame(p_data,index=c("id","t"))
pdim(pdata)
summary(pdata[c("y","x1","x2","x3")])

fixed <- plm(y~x1+x2+x3, model = "within", data=pdata)
summary(fixed)

random <- plm(y~x1+x2+x3, model = "random", data=pdata)
summary(random)

phtest(fixed,random)
