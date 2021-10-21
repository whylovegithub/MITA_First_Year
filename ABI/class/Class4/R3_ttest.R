###################################COMPARISONS

### Read library
library(MASS)
library(ggplot2)

# Generate a vector of 100 values between -4 and 4 to plot he x axis
x <- seq(-4, 4, length = 100)
x

# Simulate the t-distribution
y_1 <- dt(x, df = 4)
y_2 <- dt(x, df = 6)
y_3 <- dt(x, df = 8)
y_4 <- dt(x, df = 10)
y_5 <- dt(x, df = 1000)

# Plot the t-distributions
plot(x, y_1, type = "l", lwd = 2, xlab = "t-value", ylab = "Density", 
     main = "Comparison of t-distributions", col = "black")
lines(x, y_2, col = "red")
lines(x, y_3, col = "orange")
lines(x, y_4, col = "green")
lines(x, y_5, col = "blue")


# Add a legend
legend("topright", c("df = 4", "df = 6", "df = 8", "df = 10", "df = 1000"), col = c("black", "red", "orange", "green", "blue"), 
       title = "t-distribution", lty = 1)

#######################Compare groups
# Call a library
library(dplyr) 

auto<-`Auto.3.(1)`
# read Auto data
summary(auto)

#No summary for housepower. Convert to number   

auto$horsepower=as.numeric(auto$horsepower)  
summary(auto)
#remove missing values
auto_omit <- na.omit(auto)
dim(auto_omit)
summary(auto_omit)

auto_omit$w2<-auto_omit$weight*auto_omit$weight
lm.fit2=lm(auto_omit$mpg~auto_omit$w2+auto_omit$weight)
summary(lm.fit2)
plot(lm.fit2)
#cook distance


auto_omit$w3<-log(auto_omit$weight)
lm.fit3=lm(auto_omit$mpg~auto_omit$w3)
summary(lm.fit3)
plot(lm.fit3) 

auto_omit$dis2<-auto_omit$displacement*auto_omit$displacement
lm.fit4=lm(auto_omit$mpg~auto_omit$w3+auto_omit$displacement+auto_omit$dis2)
summary(lm.fit4)
plot(lm.fit4) 

lm.fit5=lm(auto_omit$mpg~auto_omit$w3+auto_omit$displacement+auto_omit$dis2+poly(auto_omit$horsepower,3))
summary(lm.fit5)
plot(lm.fit5) 

lm.fit5=lm(auto_omit$mpg~auto_omit$w3+auto_omit$displacement+auto_omit$dis2+poly(auto_omit$horsepower,2))
summary(lm.fit5)
plot(lm.fit5) 
#add
lm.fit6=lm(auto_omit$mpg~auto_omit$w3+auto_omit$displacement+auto_omit$dis2+poly(auto_omit$horsepower,2)+poly(auto_omit$acceleration,5))
summary(lm.fit6)
plot(lm.fit6) 
lm.fit6=lm(auto_omit$mpg~auto_omit$w3+auto_omit$displacement+auto_omit$dis2+poly(auto_omit$horsepower,2)+poly(auto_omit$acceleration,3))
summary(lm.fit6)
plot(lm.fit6) 

#get the acc2 out the system
auto_omit$acc3 <- auto_omit$acceleration*auto_omit$acceleration*auto_omit$acceleration

lm.fit7=lm(auto_omit$mpg~auto_omit$w3+auto_omit$displacement+auto_omit$dis2+poly(auto_omit$horsepower,2)+auto_omit$acceleration+auto_omit$acc3)
summary(lm.fit7)
plot(lm.fit7)
#kick acc3
lm.fit8=lm(auto_omit$mpg~auto_omit$w3+auto_omit$displacement+auto_omit$dis2+poly(auto_omit$horsepower,2)+auto_omit$acceleration)
summary(lm.fit8)
plot(lm.fit8)

lm.fit9=lm(auto_omit$mpg~auto_omit$year+auto_omit$displacement+auto_omit$dis2+poly(auto_omit$horsepower,2)+auto_omit$acceleration)
summary(lm.fit9)
plot(lm.fit9)
######################################################
#                  step forward                      #
######################################################
min.model = lm(mpg  ~ 1, data = auto_omit)
summary(min.model)

m.best.forward = step(min.model, direction='forward',
                      scope=(mpg ~weight +weight2+log(weight) + displacement+displacement2+displacement3+displacement4+displacement5 +log(displacement) 
                               + horsepower+horsepower2+horsepower3+horsepower4+horsepower5+log(horsepower) +
                               acceleration +acceleration2 +acceleration3 +acceleration4 +acceleration5 +log(acceleration) +
                               weight*horsepower), 
                      test = 'F', trace=T)
summary(m.best.forward)
plot(m.best.forward)
#######################################################







#Boston.df$rm2<-Boston.df$rm*Boston.df$rm
#lm.fit_2=lm(Boston.df$medv~Boston.df$rm2)
#summary(lm.fit_2)
#plot(Boston.df$medv~Boston.df$rm2)
#abline(lm.fit_2,lwd=3,col="red")













lapply(auto_omit,is.numeric)
unlist(lapply(auto_omit,is.numeric))

auto_omit_num <- auto_omit[,unlist(lapply(auto_omit,is.numeric))]
head(auto_omit_num)

boxplot(auto_omit_num)          #Outliers :horsepower and acceleration


#Install corrplot
library(corrplot)

corrplot.mixed(cor(auto_omit_num), order = 'AOE')   
#The stronget linear association with mpg is weight

plot(auto_omit_num$weight,auto_omit_num$mpg)       # Association has a curvature

corrplot(cor(auto_omit_num))    
#Correlations###Outliers :horsepower and acceleration

#Change cylinders to factor
auto_omit$cylinders <- as.factor(auto_omit$cylinders)

plot(auto_omit$cylinders , auto_omit$mpg,main="Car Auto Data",
     xlab="Number of Cylinders", ylab="Miles Per Gallon" )

table(auto_omit$cylinders)              ### different # cars for 4 and 6 cylinders

auto.df <- data.frame(auto_omit)
#The function data.frame() creates data frames, tightly coupled collections of variables which share many 
#of the properties of matrices and of lists, used as the fundamental data structure by most of R's modeling software.
head(auto.df)
names(auto.df)

auto.df.4 <- subset(auto.df,cylinders==4)
head(auto.df.4)

#### You can use filter also
auto_4  <- filter(auto_omit,auto_omit$cylinders==4)
head(auto_4)

auto.df.6 <- subset(auto.df,cylinders==6)
head(auto.df.6)
# one sample t test
t.test(auto.df.4$mpg, mu = 0)   #Null hypothesis Mean is equal to zero
                                #alternative hypothesys the mean is not equal to 0

t.test(auto.df.4$mpg,auto.df.6$mpg)


auto_4 <- subset(rows=auto_omit$cylinders == 4, data=auto_omit)

auto_omit$cylinders == 4,c(auto_omit$cylinders,auto_omit$mpg))

#################Boston  #############

?Boston             # medv - median value of owner-occupied homes in \$1000s.
library(MASS)
dim(Boston)
summary(Boston)

Boston
corrplot.mixed(cor(Boston), order = 'AOE')  #lstat (pct lower status population) and rm (average # rooms per dwelling have the highest linear association 

Boston$chas <- as.factor(Boston$chas)

plot(Boston$chas ,Boston$medv,main="Boston Neighbourhoods median value homes on or off river Charles",
     xlab="Off or on river Charles", ylab="Medium Value owned homes" )

Boston.df <- data.frame(Boston)
head(Boston.df)
names(Boston.df)

Boston.df.onriver <- subset(Boston.df,chas==1)
dim(Boston.df.onriver)

Boston.df.offriver <- subset(Boston.df,chas==0)
dim(Boston.df.offriver)

t.test(Boston.df.onriver$medv,Boston.df.offriver$medv)


#### Merge data sets for Project
#Merged_data_set  <- merge(dataset1, dataset2, by.x = "variale in datasest1", by.y = "variable in dataset2", all = FALSE)


##############################################################33
# Simple Linear Regression
corrplot.mixed(cor(auto_omit_num), order = 'AOE')  

lm.fit_auto=lm(mpg~weight, data = auto_omit)
summary(lm.fit_auto)

coef(lm.fit_auto)
confint(lm.fit_auto)

plot(auto_omit$weight,auto_omit$mpg,main = "Scatterplot MPG and Weight",xlab="Weight", ylab="MPG")
abline(lm.fit_auto,lwd=3,col="red")

plot(auto_omit$weight, residuals(lm.fit_auto), main = "Residuals of Weight and MPG",xlab="Weight", ylab="Residuals")

plot(lm.fit_auto)



plot(auto_omit$weight, residuals(lm.fit_auto))

################    Multiple Linear Regression
library(MASS)
library(ggplot2)

# Main problem on previous linear regression was the curve on hte instead of linear form

#Code quadratic
auto_omit$weight2 <- auto_omit$weight*auto_omit$weight

# Include quadratic
lm.fit2_auto=lm(mpg~weight+weight2,data=auto_omit)
summary(lm.fit_auto)              #Initial  R2 of 69%
summary(lm.fit2_auto)             #Quaratic R2 of 71%
plot(lm.fit2_auto)


# What was associated with mpg???
corrplot.mixed(cor(auto_omit_num), order = 'AOE')  
# displacement and horsepower are associated also with mpg

lm.fit3=lm(mpg~displacement,data=auto_omit)
summary(lm.fit3)

plot(auto_omit$displacement,auto_omit$mpg,main = "Scatterplot MPG and Displacement",xlab="Displacement", ylab="MPG")
abline(lm.fit3,lwd=3,col="red")

#Add quadratic
auto_omit$dis2 <- auto_omit$displacement*auto_omit$displacement
lm.fit4=lm(mpg~displacement+dis2,data=auto_omit)

#summary(lm.fit_auto)              #Initial  R2 of 69%
#summary(lm.fit2_auto)             #Quaratic R2 of 71%
summary(lm.fit4)                   #Quaratic R2 of 69% quadratic with higher R2 than linear
plot(lm.fit4)


#Use weight and displacement
lm.fit5=lm(mpg~weight+weight2+displacement+dis2,data=auto_omit)
summary(lm.fit5)                   #Quaratic R2 of 72% 
plot(lm.fit5)

#Add horsepower


#Using all drivers
lm.fitall6=lm(mpg~.,data=auto_omit_num)
summary(lm.fitall6)
plot(lm.fitall6)

#testing revenlant attribute
auto_omit_num$weight2 <- auto_omit_num$weight*auto_omit_num$weight
lm.fit_try=lm(mpg~.-cylinders-horsepower-acceleration+weight2-displacement,data=auto_omit_num)
summary(lm.fit_try);

#Add quadratics
lm.fitall7=lm(mpg~displacement+horsepower + weight+acceleration+weight2+dis2,data=auto_omit)
summary(lm.fitall7)
plot(lm.fitall7)

#Remove non significant variables
lm.fitall8=lm(mpg~displacement+horsepower + weight+weight2+dis2,data=auto_omit)
summary(lm.fitall8)
plot(lm.fitall8)

# Interaction Terms
summary(lm(mpg~weight*horsepower,data=auto_omit))

summary(lm(mpg~poly(weight,5),data=auto_omit))

# Non-linear Transformations of the Predictors try LOG

#Model Selection AIC  Akaike information criterion.  Lower AIC are better. The best-fit model according to AIC is the one that explains the greatest amount of variation using the fewest possible independent variables.
#Forward stepwise
min.model = lm(mpg ~ 1, data = auto_omit)
summary(min.model)

m.best.forward = step(min.model, direction='forward',
                      scope=(mpg~displacement+horsepower + weight+acceleration+weight2+dis2+weight*horsepower), 
                      test = 'F', trace=T)
#Backward
lm.fitall=lm(mpg~displacement+horsepower + weight+acceleration+weight2+dis2+weight*horsepower,data=auto_omit)
m.best.backward = step(lm.fitall, direction = 'backward', test = 'F', trace=T)

summary(m.best.backward)
summary(m.best.forward)

plot(m.best.backward)
plot(m.best.forward)

