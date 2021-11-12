library(MASS)
library(dplyr)
library(corrplot)
library(fastDummies)
library(ROCR)
library(class)

#look at the data
data(Pima.te)
?Pima.te
summary(Pima.te)
adim(Pima.te)

#split the set to training and testing
Pima.te.df = Pima.te

#Data partitioning
set.seed(100)

#80% for training data and 20% for testing model using sample.split
spl = sample.split(Pima.te.df$type_Yes, SplitRatio = 0.8)
train = subset(Pima.te.df, spl==TRUE)     #Training set 80% data
test = subset(Pima.te.df, spl==FALSE)     #Testing set 20% data
print(dim(train)); print(dim(test))

#Baseline Accuracy
prop.table(table(train$type_Yes))     # 67% UP on training set
prop.table(table(test$type_Yes))      # 67% UP on testing set 


#Dummies
Pima.te.df <- dummy_cols(Pima.te.df,select_columns = "type")
Pima.te.df

#GLM
glm1 = glm(type_Yes ~ npreg+glu+bp+skin+bmi+ped+age, data = Pima.te.df, family = "binomial")
glm1
summary(glm1)
par(mfrow=c(2,3))
boxplot(npreg~type,data=Pima.te.df)
boxplot(glu~type,data=Pima.te.df)
boxplot(skin~type,data=Pima.te.df)
boxplot(bmi~type,data=Pima.te.df)
boxplot(ped~type,data=Pima.te.df)
boxplot(age~type,data=Pima.te.df)

#forward
min.model = glm(type_Yes ~ 1,family=binomial, data = Pima.te.df)
forwards_1 = step(min.model,
                scope = type_Yes ~ npreg+glu+bp+skin+bmi+ped+age, direction="forward",
                family=binomial, data = Pima.te.df,family=binomial)
summary(forwards_1)
plot(forwards_1)

forwards_2 = step(min.model,
                scope = type_Yes ~ npreg+glu+bp+skin+bmi+ped+age+poly(glu,5)+poly(bmi,5)
                +poly(ped,5)+poly(npreg,5), direction="forward",
                family=binomial, data = Pima.te.df,family=binomial)
summary(forwards_2)



##########################################################################################################
#                                     there is clear pattern                                             #
##########################################################################################################

model_glm = glm(type~. - type_Yes - type_No -age -skin -bp,data = train,family=binomial)
summary(model_glm)                   # 4 significant drivers
glm_best=glm(type_Yes ~ glu + npreg + bmi + ped,data = train)
#Predictions on the test set
predictTest = predict(glm_best, newdata = test, type = "response")
pred = prediction(predictTest, test$type)
perf = performance(pred,"tpr","fpr")
pred
plot(perf,colorize=TRUE)

# Confusion matrix on test set
table(test$type, predictTest >= 0.5)
table(test$type, predictTest >= 0.46)

##########################################################################################################
#                                              LDA                                                       #
##########################################################################################################

model_LDA = lda(type~. - type_Yes - type_No,data = train)
plot(model_LDA)

#extract the class out of the predict()
predictTest2 = predict(model_LDA, newdata = test, type="response")
tab2 = table(predictTest2$class,test$type)
tab2
sum(diag(tab2))/sum(tab2)


#LDA2
model_LDA2 = lda(type~. - type_Yes - type_No - skin - bp,data = train)
predictTest3 = predict(model_LDA2, newdata = test, type="response")
tab3 = table(predictTest3$class,test$type)
tab3
sum(diag(tab3))/sum(tab3)

#LDA3
model_LDA3 = lda(type~. - type_Yes - type_No - skin - bp -age,data = train)
predictTest4 = predict(model_LDA3, newdata = test, type="response")
tab4 = table(predictTest4$class,test$type)
tab4
sum(diag(tab4))/sum(tab4)

##########################################################################################################
#                                              QDA                                                       #
##########################################################################################################


model_QDA = qda(type~. - type_Yes - type_No - skin - bp -age,data = train)
predictTest_qda = predict(model_QDA, newdata = test, type="response")
tab_qda = table(predictTest_qda$class,test$type)
tab_qda
sum(diag(tab_qda))/sum(tab_qda)

##########################################################################################################
#                                              k-nn                                                      #
##########################################################################################################
Pima.te.df$type = as.numeric(Pima.te.df$type)-1

train <- sample(1:332, 332*0.8) 
Train = cbind(Pima.te.df$npreg,Pima.te.df$glu,Pima.te.df$bp,Pima.te.df$skin,Pima.te.df$bmi,Pima.te.df$ped,Pima.te.df$age)[train,]
Test = cbind(Pima.te.df$npreg,Pima.te.df$glu,Pima.te.df$bp,Pima.te.df$skin,Pima.te.df$bmi,Pima.te.df$ped,Pima.te.df$age)[-train,]
dim(Test)
dim(Train)
Train_type = Pima.te.df$type_Yes[train]
Test_type = Pima.te.df$type_Yes[-train]


?knn
predictTest_knn = knn(Train, Test, Train_type ,k = 16)
tab_knn = table(Test_type,predictTest_knn)
tab_knn
(tab_knn[1,1]+tab_knn[2,2])/nrow(Test)

predictTest_knn2 = knn(Train, Test, Train_type ,k = 19)
tab_knn2 = table(Test_type,predictTest_knn2)
tab_knn2
(tab_knn2[1,1]+tab_knn2[2,2])/nrow(Test)

predictTest_knn3 = knn(Train, Test, Train_type ,k = 20)
tab_knn3 = table(Test_type,predictTest_knn3)
tab_knn3
(tab_knn3[1,1]+tab_knn3[2,2])/nrow(Test)

predictTest_knn4 = knn(Train, Test, Train_type ,k = 23)
tab_knn4 = table(Test_type,predictTest_knn4)
tab_knn4
(tab_knn4[1,1]+tab_knn4[2,2])/nrow(Test)
























