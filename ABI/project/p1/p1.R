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
plot(lm.fit_1)

Boston.df$rm2<-Boston.df$rm*Boston.df$rm
lm.fit_2=lm(Boston.df$medv~Boston.df$rm2+Boston.df$rm)
plot(lm.fit_2)

Boston.df$rm3<-log(Boston.df$rm)


#Add quadratic
lm.fit_3=lm(Boston.df$medv~Boston.df$rm3)
summary(lm.fit_3)
plot(lm.fit_3)

lm.fit_31=lm(medv~rm+rm2+rm3,data=Boston.df)
summary(lm.fit_31)
plot(lm.fit_31)

lm.fit_32=lm(medv~rm+rm3,data=Boston.df)
summary(lm.fit_32)
plot(lm.fit_32)

#Add lstat

lm.fit_4=lm(medv~rm+rm3+lstat,data=Boston.df)
summary(lm.fit_4)
plot(lm.fit_4)

#add ptratio
lm.fit_5=lm(medv~rm+rm3+lstat+ptratio,data=Boston.df)
summary(lm.fit_5)
plot(lm.fit_5)

#add age and nox
lm.fit_6=lm(medv~rm+rm3+lstat+ptratio+age+nox,data=Boston.df)
summary(lm.fit_6)
plot(lm.fit_6)

#add dis
lm.fit_7=lm(medv~rm+rm3+lstat+ptratio+age+nox+dis,data=Boston.df)
summary(lm.fit_7)
plot(lm.fit_7)

#add 1/indus since the indus shows the non business land so its ^-1 show's the porp
x <- 1/Boston.df$indus
lm.fit_8=lm(medv~rm+rm3+lstat+ptratio+age+nox+dis+x,data=Boston.df)
summary(lm.fit_8)
plot(lm.fit_8)
lm.fit_81=lm(medv~rm+rm3+lstat+ptratio+nox+dis+x,data=Boston.df)
summary(lm.fit_81)
plot(lm.fit_81)

#coefficients(lm.fit_8)
#add crime rate and black

lm.fit_9=lm(medv~rm+rm3+lstat+ptratio+nox+dis+x+crim+black,data=Boston.df)
summary(lm.fit_9)
plot(lm.fit_9)

#add zn and no use
zn_1 <- 1-Boston.df$zn/100

lm.fit_9=lm(medv~rm+rm3+lstat+ptratio+nox+dis+x+crim+black+zn,data=Boston.df)
summary(lm.fit_9)
plot(lm.fit_9)

#add rad and test
lm.fit_10=lm(medv~rm+rm3+lstat+ptratio+nox+dis+x+crim+black+rad+tax,data=Boston.df)
summary(lm.fit_10)
plot(lm.fit_10)
#add poly(rm,5) and Ploy(lstat,5) and kick rm and lstat since it include in the poly
lm.fit_11=lm(medv~rm3+poly(rm,5)+poly(lstat,5)+ptratio+nox+dis+x+crim+black+rad+tax,data=Boston.df)
summary(lm.fit_11)
plot(lm.fit_11)

lm.fit_12=lm(medv~rm3+poly(rm,4)+poly(lstat,4)+ptratio+nox+dis+x+crim+black+rad+tax,data=Boston.df)
summary(lm.fit_12)
plot(lm.fit_12)

#kick x and lstat3
Boston.df$lstat2=Boston.df$lstat*Boston.df$lstat
Boston.df$lstat4=Boston.df$lstat*Boston.df$lstat*Boston.df$lstat*Boston.df$lstat
lm.fit_13=lm(medv~rm3+poly(rm,4)+lstat+lstat2+lstat4+ptratio+nox+dis+crim+black+rad+tax,data=Boston.df)
summary(lm.fit_13)
plot(lm.fit_13)

#kick lstat4
Boston.df$lstat2=Boston.df$lstat*Boston.df$lstat
Boston.df$lstat4=Boston.df$lstat*Boston.df$lstat*Boston.df$lstat*Boston.df$lstat
lm.fit_14=lm(medv~rm3+poly(rm,4)+lstat+lstat2+ptratio+nox+dis+crim+black+rad+tax,data=Boston.df)
summary(lm.fit_14)
plot(lm.fit_14)

########################################################################################################
##################################we are now doing life expendency######################################
########################################################################################################

 
LE <- read.csv("C:/Users/whylo/Desktop/MITA_First_Year/ABI/project/p1/LE.csv",header = TRUE, sep = ",", quote = "\"",
               dec = ".", fill = TRUE, comment.char = "" )

summary(LE);
le = na.omit(LE);
summary(le);
names(le)[0]= "Country";

#kick the first column out to fit the numerical requirement of the function.

le_cor = le[,2:12]
corrplot.mixed(cor(le_cor), order = 'AOE');

#As we can see on the plot that FR,YC,MA,UB has strong relation ship with BS(both sex of life expectancy)
#starting with MA
lm.le_1=lm(BS~MA, data = le)
summary(lm.le_1)
plot(lm.le_1)

#add MA^2
MA2 = le$MA*le$MA;
lm.le_11=lm(BS~MA+MA2, data = le)
summary(lm.le_11)
plot(lm.le_11)

#add log(MA)
MA0 = log(le$MA);
lm.le_12=lm(BS~MA0+MA+MA2, data = le)
summary(lm.le_12)
plot(lm.le_12)

#log(MA) leaving and add FR
lm.le_2=lm(BS~MA+MA2+FR, data = le)
summary(lm.le_2)
plot(lm.le_2)

lm.le_21=lm(BS~MA+FR, data = le)
summary(lm.le_21)
plot(lm.le_21)

# adding Yc
lm.le_3=lm(BS~MA+MA2+FR+YC, data = le)
summary(lm.le_3)
plot(lm.le_3)
lm.le_31=lm(BS~MA+MA2+YC, data = le)
summary(lm.le_31)
plot(lm.le_31)

#here is the optimal system on step 3
lm.le_32=lm(BS~MA+FR+YC, data = le)
summary(lm.le_32)
plot(lm.le_32)

#adding UB
lm.le_4=lm(BS~MA+FR+YC+UB, data = le)
summary(lm.le_4)
plot(lm.le_4)

#add M
lm.le_5=lm(BS~MA+FR+YC+UB+M, data = le)
summary(lm.le_5)
plot(lm.le_5)

lm.le_6=lm(BS~poly(MA,5)+poly(FR,5)+poly(YC,5)+poly(UB,5), data = le)
summary(lm.le_6)
plot(lm.le_6)

#ma2~5 leaving,fr2 leaving,ub3~5 leaving
FR3=le$FR*le$FR*le$FR
FR4=le$FR*le$FR*le$FR*le$FR
lm.le_7=lm(BS~MA+FR+FR3+FR4+poly(YC,5)+poly(UB,2), data = le)
summary(lm.le_7)
plot(lm.le_7)

#YC2~5 leaving
lm.le_8=lm(BS~MA+FR+FR3+FR4+YC+poly(UB,2), data = le)
summary(lm.le_8)
plot(lm.le_8)

#fr leaving
lm.le_9=lm(BS~MA+FR3+FR4+YC+poly(UB,2), data = le)
summary(lm.le_9)
plot(lm.le_9)


