Covid <- read.csv("C:/Users/whylo/Desktop/Homework/ABI/hw2/Covid.csv")
covid<-Covid

#summary the dataset
summary(covid)

names(covid)[names(covid) == "COVID.19.Deaths"] <- "CD"
names(covid)[names(covid) == "Total.Deaths"] <- "TD"
names(covid)[names(covid) == "Age.Group"] <- "AG"
names(covid)[names(covid) == "Group"] <- "G"


Covid_njfl<-sqldf("select Month, State, CD, TD 
                   from covid 
                   where G = 'By Month' and Sex ='All Sexes' and AG='All Ages' and State IN ('New Jersey','Florida')")

library(dplyr)
summary(Covid_njfl)
Covid_njfl$CD <- gsub (",","", Covid_njfl$CD)
Covid_njfl$TD <- gsub (",","", Covid_njfl$TD)

Covid_njfl$CD <- as.numeric(as.character(Covid_njfl$CD))
Covid_njfl$TD <- as.numeric(as.character(Covid_njfl$TD))
#0 as fl, 1 as nj.

aggregate(Covid_njfl$CD/Covid_njfl$TD,
          by = list(State = Covid_njfl$State),
          mean)
aggregate(Covid_njfl$CD/Covid_njfl$TD,
          by = list(State = Covid_njfl$State),
          median)
proportions <- Covid_njfl$CD/Covid_njfl$TD
# Compute the analysis of variance. Are all MPG for all cylinders tuypes the same?
res_Covid_njfl.aov <- aov(Covid_njfl$CD/Covid_njfl$TD ~ State, data = Covid_njfl)
summary(res_Covid_njfl.aov)
?boxplot

TukeyHSD(res_Covid_njfl.aov)

Covid_njfl$State <- gsub ("Florida",0, Covid_njfl$State)
Covid_njfl$State <- gsub ("New Jersey",1, Covid_njfl$State)
Covid_njfl$State <- as.numeric(as.character(Covid_njfl$State))
typeof(Covid_njfl$State)
boxplot(Covid_njfl$State,Covid_njfl$CD/Covid_njfl$TD)

