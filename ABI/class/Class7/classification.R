library(ISLR)
library(caTools)
library(caret)
library(plyr)
library(readr)
library(dplyr)

?Smarket
summary(Smarket)
boxplot(Today~Direction,data=Smarket, main="Today by Gain or Loss")  
boxplot(Volume~Year,data=Smarket, main="")
boxplot(Today~Year,data=Smarket, main="")

#Dummies
library(fastDummies)

Smarket <- dummy_cols(Smarket,select_columns = "Direction")
Smarket
#glm()genaral linear model
mylogit <-  glm(Direction_Up ~ Lag1 + Lag2 + Lag3 + Lag4 +Volume, data = Smarket, family = "binomial")
summary(mylogit)

# Forwards
min.model <- glm(Direction_Up ~ 1,family=binomial, data= Smarket)
summary(min.model)

forwards = step(min.model,
                scope = Direction_Up ~ Lag1 + Lag2 + Lag3 + Lag4 +Volume, direction="forward",
                family=binomial, data= Smarket)
# Backward
backwards = step(mylogit)



library(plyr)
library(readr)
library(dplyr)
library(caret)
library(caTools)

#Data partitioning
set.seed(100)

#80% for training data and 20% for testing model using sample.split
spl = sample.split(Smarket$Direction_Up, SplitRatio = 0.8)
train = subset(Smarket, spl==TRUE)     #Training set 80% data
test = subset(Smarket, spl==FALSE)     #Testing set 20% data
print(dim(train)); print(dim(test))

contrasts(Smarket$Direction)  # No dummies are needed

#Baseline Accuracy
prop.table(table(train$Direction_Up))     # 52% UP on training set
prop.table(table(test$Direction_Up))      # 52% UP on testing set 


#Model Direction with glm Binomila distribution on all variables except Today
model_glm = glm(Direction ~ . -Today -Direction_Up -Direction_Down ,family=binomial,  data = train)
summary(model_glm)                   # 2 significant drivers



#Predictions on the test set
predictTest = predict(model_glm, newdata = test, type = "response")
# Confusion matrix on test set
table(test$Direction, predictTest >= 0.5)

table(test$Direction, predictTest >= 0.53)
