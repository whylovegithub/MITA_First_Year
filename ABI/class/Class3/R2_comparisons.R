Auto <- read.csv("/Users/ouhiroshisakai/Downloads/Auto-3.csv")
#View(Auto)

#R is case sensitive (advise to use all capitals or all lower
auto <- Auto
#######################Compare groups
# Call a library
library(dplyr)

#do you still have auto in your memory?
dim(auto)
auto$cylinders=as.factor(auto$cylinders)
#Check mean of mpg by cylinder
aggregate(Auto$mpg,
          by = list(cylinders = Auto$cylinders),
          mean)

aggregate(Auto$mpg,
          by = list(cylinders = Auto$cylinders),
          median)
          
# Compute the analysis of variance. Are all MPG for all cylinders tuypes the same?
          res_auto.aov <- aov(mpg ~ cylinders, data = auto)
          summary(res_auto.aov)
          
#Tukey multiple pairwise-comparisons
#As the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest 
#   Significant Differences, R function: TukeyHSD()) for 
#performing multiple pairwise-comparison between the means of #groups.
#The function TukeyHD() takes the fitted ANOVA as an argument.
         
plot(auto$cylinders,auto$mpg,xlab = "number of cylinders",ylab = "miles per gasoline");
TukeyHSD(res_auto.aov)
          
    
          
          
          
          