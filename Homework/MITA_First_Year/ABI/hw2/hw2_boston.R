library(MASS)
Boston
?Boston

boston<-Boston
#Is the medium value of owner-occupied homes different between the neighborhoods next to the Charles river 
#and the ones not next to the river?


#summarize the data first
summary(boston)

#Compare groups
# Call a library
library(dplyr)
dim(boston)
boston$chas = as.factor(boston$chas)
#Check mean of medv by chas.
aggregate(Boston$medv,
          by = list(chas = Boston$chas),
          mean)

aggregate(Boston$medv,
          by = list(chas = Boston$chas),
          median)

# Compute the analysis of variance. Are all MPG for all cylinders tuypes the same?
res_boston.aov <- aov(medv ~ chas, data = boston)
summary(res_boston.aov)
plot(boston$chas,boston$medv,xlab = "did the appartment next to charles river",ylab="median value of owner-occupied homes in $1000s.")
TukeyHSD(res_boston.aov)










