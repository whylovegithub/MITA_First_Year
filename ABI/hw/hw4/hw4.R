library(lubridate)
library(dplyr)
library(corrplot)
library(fastDummies)

ad = Advertisement_State
ad
datetxt <- ad$Date
datetxt <- as.Date(datetxt)
date <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))
ad$year = date$year
ad$month = date$month
ad.df = select(ad,c(1,7,8,3,4,5,6))

#investment and sale
is <- ad.df[,4:7]

corrplot.mixed(cor(is), order = 'AOE')

pairs(is)

dummy_month <- dummy_cols(ad.df,select_columns = "month")
ad.df <- dummy_month

ad.df$quarter_1=0
ad.df$quarter_2=0
ad.df$quarter_3=0
ad.df$quarter_4=0
for(i in 1:nrow(ad.df)) {
  row <- ad.df[i,]
  if(ad.df[i,]$month == 1 || ad.df[i,]$month == 2 ||ad.df[i,]$month == 3){
    ad.df[i,]$quarter_1 =1
  }
  if(ad.df[i,]$month == 4 || ad.df[i,]$month == 5 ||ad.df[i,]$month == 6){
    ad.df[i,]$quarter_2 = 1
  }
  if(ad.df[i,]$month == 7 || ad.df[i,]$month == 8 ||ad.df[i,]$month == 9){
    ad.df[i,]$quarter_3 = 1
  }
  if(ad.df[i,]$month == 10 || ad.df[i,]$month == 11 ||ad.df[i,]$month == 12){
    ad.df[i,]$quarter_4 = 1
  }
}

write.csv(ad.df,"C:\\Users\\whylo\\Desktop\\MITA_First_Year\\ABI\\hw\\hw4\\ad.df.csv", row.names = TRUE)
ad.df.train <- subset(ad.df,year<=2018)
head(ad.df.train)
dim(ad.df.train)

ad.df.test <- subset(ad.df,year>=2019)
head(ad.df.test)
dim(ad.df.test)

min.model = lm(sales_state ~ 1, data = ad.df)
forward = step(min.model,
               scope = sales_state ~ quarter_1+quarter_2+quarter_3+quarter_4,
               data = ad.df)
summary(forward)
coefficients(forward)
