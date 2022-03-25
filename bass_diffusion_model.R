library(tidyverse)
library(data.table)

movie_df <- read.csv("./data/movie.csv")
head(movie_df,17)
#lag data가 다른 것은, veteran_cum_lag, 데이터 자체가 시사회등을 반영해서 더해져있는 것임

movie_df.nls1 <- nls(amsal ~ (p+q*amsal_cum_lag/m) * (m-amsal_cum_lag) * (1+b1*fri+b2*(sat+sun))
                  ,start=list(p=0.1,q=0.2,m=13000000,b1=0.2,b2=0.5)
                  ,data=movie_df )
summary(movie_df.nls1)                  
#월~목에 비해서, 금요일 11%, 토,일은 89% 증가
#p가 오히려 q보다 크다, 특이한 케이스 -> 혁신수용자가 모방자보다 많다.
names(movie_df)
formula <- veteran ~ (p+q*veteran_cum_lag/m) * (m-veteran_cum_lag) * (1+b1*fri+b2*(sat+sun) )
movie_df.nls2 <- nls(formula
                     ,start=list(p=0.1,q=0.2,m=13000000,b1=0.2,b2=0.5)
                     ,data=movie_df )
               
options(scipen=999)
summary(movie_df.nls2)   


# get coefficient

p <- coef(movie_df.nls1 )[1]
q <- coef(movie_df.nls1 )[2]
m <- coef(movie_df.nls1 )[3]
b1 <- coef(movie_df.nls1 )[4]
b2 <- coef(movie_df.nls1 )[4]

# setting the starting value for M to the recorded total sales.
pred_df <- (p+q*movie_df$amsal_cum_lag/m) * (m-movie_df$amsal_cum_lag) * (1+b1*movie_df$fri+b2*(movie_df$sat+movie_df$sun) ) 

raw_df <- data.table(cbind(movie_df$obs,pred_df))
names(raw_df) <- c('std_dt','viewers')
raw_df <- raw_df[!is.na(raw_df$viewers)]

raw_df$viewers <- as.double(raw_df$viewers)
glimpse(raw_df)
# plot pdf
sum(raw_df$viewers)

sum(movie_df$amsal,na.rm=TRUE)


raw_df$viewers
movie_df$veteran
