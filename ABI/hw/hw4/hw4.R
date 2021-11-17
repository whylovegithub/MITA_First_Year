library(lubridate)
library(dplyr)
library(corrplot)
library(fastDummies)
library(sjstats)
library(class)
library(MASS)
library(ROCR)

ad = Advertisement_State
ad
#create years and month
datetxt <- ad$Date
datetxt <- as.Date(datetxt)
date <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))


ad$year = date$year
ad$month = date$month

#create season
ad$quarter=0
ad
for(i in 1:nrow(ad.df)) {
  row <- ad[i,]
  if(ad[i,]$month == 1 || ad[i,]$month == 2 ||ad[i,]$month == 3){
    ad[i,]$quarter =1
  }
  if(ad[i,]$month == 4 || ad[i,]$month == 5 ||ad[i,]$month == 6){
    ad[i,]$quarter = 2
  }
  if(ad[i,]$month == 7 || ad[i,]$month == 8 ||ad[i,]$month == 9){
    ad[i,]$quarter = 3
  }
  if(ad[i,]$month == 10 || ad[i,]$month == 11 ||ad[i,]$month == 12){
    ad[i,]$quarter = 4
  }
}
#create adstocks
ad$tv_add=0
ad$radio_add=0
ad$news_add=0
ad[1,]$tv_add = ad[1,]$TV_State/1.5
ad[1,]$radio_add = ad[1,]$Radio_State/1.5
ad[1,]$news_add= ad[1,]$News_State/1.5
for(i in 2:nrow(ad.df)) {
  row <- ad[i,]
  ad[i,]$tv_add = (ad[i,]$TV_State+0.5*ad[i-1,]$TV_State)/1.5
  ad[i,]$radio_add = (ad[i,]$Radio_State+0.5*ad[i-1,]$Radio_State)/1.5
  ad[i,]$news_add = (ad[i,]$News_State+0.5*ad[i-1,]$News_State)/1.5
  
}
ad
ad.df = select(ad,c(1,7,8,9,3,4,5,6,10,11,12))
ad.df
#investment and sale
is <- ad.df[,5:8]

corrplot.mixed(cor(is), order = 'AOE')

pairs(is)

dummy_month <- dummy_cols(ad.df,select_columns = "month")
ad.df <- dummy_month

dummy_quarter <- dummy_cols(ad.df,select_columns = "quarter")
ad.df <- dummy_quarter

dummy_state <- dummy_cols(ad.df,select_columns = "State")
ad.df <- dummy_state

dim(ad.df)


write.csv(ad.df,"C:\\Users\\whylo\\Desktop\\MITA_First_Year\\ABI\\hw\\hw4\\ad.df.csv", row.names = TRUE)
ad.df.train <- subset(ad.df,year<=2018)
head(ad.df.train)
dim(ad.df.train)

ad.df.test <- subset(ad.df,year>=2019)
head(ad.df.test)
dim(ad.df.test)

min.model = lm(sales_state ~ 1, data = ad.df)
forward = step(min.model,
               scope = sales_state ~ quarter_1+quarter_2+quarter_3+quarter_4+
                 State_CT+State_DL+State_MD+State_NJ+State_NY+State_PA,
               data = ad.df)
summary(forward)
coefficients(forward)

min.model = lm(log(sales_state) ~ 1, data = ad.df)
forward2 = step(min.model,
               scope = log(sales_state) ~ quarter_1+quarter_2+quarter_3+quarter_4+
                 State_CT+State_DL+State_MD+State_NJ+State_NY+State_PA,
               data = ad.df)
summary(forward2)
coefficients(forward2)

model_3 = glm(formula = sales_state~quarter_1+quarter_2+quarter_3+quarter_4+
                State_CT+State_DL+State_MD+State_NJ+State_NY+State_PA+tv_add+radio_add+news_add
              ,data = ad.df)
summary(model_3)

mse(model_3)

mean_tv = mean(ad.df$tv_add)*0.2
mean_radio = mean(ad.df$radio_add)*0.2
mean_news = mean(ad.df$news_add)*0.2

model_4 = glm(formula = log(sales_state)~quarter_1+quarter_2+quarter_3+quarter_4+
                State_CT+State_DL+State_MD+State_NJ+State_NY+State_PA
              +log(tv_add+mean_tv)+log(radio_add+mean_radio)+log(news_add+mean_news)
              ,data = ad.df.train)
summary(model_4)
prediction_4 = predict(model_4, ad.df.test)
summary(prediction_4)
coefficients(model_4)
