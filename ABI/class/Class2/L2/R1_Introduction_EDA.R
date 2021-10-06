#R1_Introduction to learn how to import data ssets and EDA

#You can install the complete tidyverse with a single line of code
yes

#1. Download the Auto Data Set that is in CANVAS
#2. Save it
#3.Import Auto Dataset using the "Import Datset" and the "From text" because 
#the Auto is a csv file
Auto <- read.csv("/Users/ouhiroshisakai/Downloads/ABI/Class2/L2/Auto.csv")
#View(Auto)

#R is case sensitive (advise to use all capitals or all lower
auto <- Auto  

#shows the first rows of the data set
head(auto)       

#shows the first 25 rows of the data set and all the columns
auto[1:25,]

#shows the first 5 rows of the data set and the first 3 columns
auto[1:5,1:3]

#shows the fnames of the variables
names(auto)

# shows the 5# Summary, the mean and # missing values for quantitative variables 
#and frequencies for categorical variables
summary(auto)
#discuss cylinders. Is it a categorial or numerical variable?
#The engine displacement of a car is the combined volume of all its cylinders
#discuss is horsepower categorical or numeric?

#Convert horsepower to numeric 
auto$horsepower=as.numeric(auto$horsepower)
summary(auto)

#Check Shape - Plot histograms and boxplots for mpg
hist(auto$mpg)
boxplot(auto$mpg)
#Compute Location, variability and missing
summary(auto$mpg)

boxplot(auto)
boxplot(auto[1:8])

#compute the mean, median and standard deviation
mean(auto$mpg)
median(auto$mpg)
sd(auto$mpg)

pairs(auto[1:8])
#2 by 2 relationships, What is the horsepower more related to?

#Remove the missing values
auto_omit <- na.omit(auto) 

summary(auto_omit)

cor(auto_omit[1:8])

plot(auto$cylinders , auto$mpg )

#convert cylinders to categorical
auto$cylinders=as.factor(auto$cylinders)

plot(auto$cylinders , auto$mpg )   #Waht is x? what is Y?
plot(auto$cylinders, auto$mpg ,xlab="Number of cylinders",ylab="Miles per Gallon",main="Miles per Gallon per Number of Cylinders")

plot(auto$weight, auto$mpg ,xlab="Car Weight",ylab="Miles per Gallon",main="Miles per Gallon per the Auto Weight")

cor(auto$weight,auto$mpg)

###############          Read Boston dataset  ################################
######Boston data is in the MASS library
######1.Make sure you havae the MASS library, otherwise install it
library(MASS)

#Description of Boston data set
?Boston

head(Boston)

head(Boston,2)                # Shows 2 rows

names(Boston)

#Rename variables
names(Boston)[names(Boston) == "zn"] <- "prop_res"

names(Boston)

#Number of rows and columns
dim(Boston)
dim(auto)

#Type of variables: character (categorical), numerical, integer
lapply(Boston, class)

#Variable values
unique(Boston$rad)

summary(Boston)

boxplot(Boston$crim)
boxplot(Boston$prop_res)

# which data sets do I have available after using the MASS library?
data()

# Check th order and combine the following data sets
state.abb                          
state.area 
?state.area 
state.center 
state.division 
state.name 
state.region 
state.x77

state.x78 <- cbind(state.abb, state.name, state.region, state.division, 
                   state.center$x, state.center$y, state.area)

state.x78

##############Export data serts froom R   #############
write.csv(Boston, "/Users/ouhiroshisakai/Downloads/ABI/Class2/L2/Boston.csv")
data(anorexia)
data(Pima.te)
