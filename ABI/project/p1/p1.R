library(MASS)
library(corrplot)
library(dplyr) 

summary(Boston)
head(Boston)
str(Boston)


Boston$chas <- as.numeric(as.character(Boston$chas))
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
lm.fit_1=lm(medv~rm, data = Boston.df)
summary(lm.fit_1)
coef(lm.fit_rm)
confint(lm.fit_1)
plot(Boston.df$rm, Boston.df$medv, main="the scatter plot of rm and medv",xlab="rm",ylab="medv")
abline(lm.fit_1,lwd=3,col="red")

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

#add ptratio
lm.fit_5=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm+Boston.df$lstat+Boston.df$ptratio)
summary(lm.fit_5)
plot(lm.fit_5)



lm.fit_6=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm+Boston.df$lstat+Boston.df$ptratio+Boston.df$age+Boston.df$nox)
summary(lm.fit_6)
plot(lm.fit_6)

lm.fit_7=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm+Boston.df$lstat+Boston.df$ptratio+Boston.df$age+Boston.df$nox+Boston.df$dis)
summary(lm.fit_7)
plot(lm.fit_7)

#kick age out and add 1/indus since the indus shows the non business land so its ^-1 show's the porp
x <- 1/Boston.df$indus
lm.fit_8=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm+Boston.df$lstat+Boston.df$ptratio+Boston.df$nox+Boston.df$dis+x)
summary(lm.fit_8)
plot(lm.fit_8)

#coefficients(lm.fit_8)
#add crime rate and black

lm.fit_9=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm+Boston.df$lstat+Boston.df$ptratio+Boston.df$nox+Boston.df$dis+x+Boston.df$crim+Boston.df$black)
summary(lm.fit_9)
plot(lm.fit_9)

#add zn and no use
zn_1 <- 1-Boston.df$zn/100

lm.fit_9=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm+Boston.df$lstat+Boston.df$ptratio+Boston.df$nox+Boston.df$dis+x+Boston.df$crim+Boston.df$black+zn_1)
summary(lm.fit_9)
plot(lm.fit_9)

lm.fit_9=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm+Boston.df$lstat+Boston.df$ptratio+Boston.df$nox+Boston.df$dis+x+Boston.df$crim+Boston.df$black+Boston.df$rad)
summary(lm.fit_9)
plot(lm.fit_9)

lm.fit_9=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm+Boston.df$lstat+Boston.df$ptratio+Boston.df$nox+Boston.df$dis+x+Boston.df$crim+Boston.df$black+Boston.df$rad+Boston.df$tax)
summary(lm.fit_9)
plot(lm.fit_9)

















