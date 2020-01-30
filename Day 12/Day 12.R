
#Day 12 of 100DaysOfCode Twitter Challenge

rm(list = ls(all=TRUE))
cat('\014')

install.packages(c('readxl','caTools', 'caret'))


library(readxl)
library(caret)
library(caTools)


########### Load my data

getwd()
path <- 'C:/Users/mwm9/Desktop/100DaysOfCode/100DaysCodeChallenge/Day 12'
setwd(path)
list.files()


#load data and check structure
Titanic1 <- read.csv('train.csv', header=TRUE, na.strings = c(""))
str(Titanic1)
View(Titanic1)


#Check for missing data
sapply(Titanic1, function(x) sum(is.na(x)))

hist(Titanic1$Age, breaks = 15, freq = F, xlab = 'Age', ylab = 'Density', main = 'Age Density Plot',col = "orange")
lines(density(Titanic1$Age, na.rm = T),col="blue",xlim=c(-10,85),lwd=2)


mean(Titanic1$Age,na.rm = T)

median(Titanic1$Age,na.rm = T)

summary(Titanic1$Embarked)



# Mode imputation
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v,uniqv)))]
}

v <- Titanic1$Embarked
mode <- getmode(v)
mode





Titanic1$Age[is.na(Titanic1$Age)]<- median((Titanic1$Age),na.rm=T)
Titanic1$Embarked[is.na(Titanic1$Embarked)]<- mode



