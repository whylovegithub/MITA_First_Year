library(MASS)
library(corrplot)
library(dplyr) 

summary(Boston)
head(Boston)
str(Boston)



corrplot.mixed(cor(Boston), order = 'AOE')

#As the plot shows the medv shows a strong relationship with the rm at 0.7, with the lstat at-0.74
#the ptratio at -0.51, several at aroud .40s


Boston$chas <- as.factor(Boston$chas)
Boston$rm <- as.numeric(as.character(Boston$rm))
Boston.df <- data.frame(Boston)
head(Boston.df)
names(Boston.df)

summary(Boston.df$rm)

plot(Boston.df$rm, Boston.df$medv, main="the scatter plot of rm and medv",xlab="rm",ylab="medv")
lm.fit_rm=lm(medv~rm, data = Boston.df)
summary(lm.fit_rm)
coef(lm.fit_rm)
confint(lm.fit_rm)
plot(Boston.df$rm, Boston.df$medv, main="the scatter plot of rm and medv",xlab="rm",ylab="medv")
abline(lm.fit_rm,lwd=3,col="red")

Boston.df$rm2<-Boston.df$rm*Boston.df$rm
lm.fit_2=lm(Boston.df$medv~Boston.df$rm2)
summary(lm.fit_2)
plot(Boston.df$medv~Boston.df$rm2)
abline(lm.fit_2,lwd=3,col="red")

lapply(Boston.df,is.numeric)
unlist(lapply(Boston.df,is.numeric))

#Add quadratic
lm.fit_3=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm)
summary(lm.fit_3)
plot(lm.fit_3)

#Add lstat

lm.fit_4=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm+Boston.df$lstat)
summary(lm.fit_4)
plot(lm.fit_4)













