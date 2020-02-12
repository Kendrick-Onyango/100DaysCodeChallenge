rm(list = ls(all=TRUE))
cat('\014')


path <- 'C:/Users/Mustapha/Desktop/100DaysofCode/100DaysCodeChallenge/Day 21'
setwd(path)
list.files()



library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(mice)
library(randomForest)


train_data <- read.csv('train.csv', stringsAsFactors = FALSE)
test_data <- read.csv('test.csv', stringsAsFactors = FALSE)

combined_data <- bind_rows(train_data, test_data)

#Check data structure
glimpse(combined_data)
str(combined_data)


dim(train_data)
dim(test_data)

# Feature Engineering
combined_data$Title <- gsub('(.*,) | (\\ ..*)', '', combined_data$Name)

# show title counts by sex
table(combined_data$Sex, combined_data$Title)
table(combined_data$Title, combined_data$Sex)

#Titles with very low cell counts to be combined to 'rare' level
rare_title <- c('Dona','Lady','the Countess', 'Capt','Col','Don','Dr',
                'Major','Rev','Sir','Jonkheer')


# Reassing mlle, ms, and mme accordingly
combined_data$Title[combined_data$Title == 'Mlle'] <- 'Miss'
combined_data$Title[combined_data$Title == 'Ms'] <- 'Miss'
combined_data$Title[combined_data$Title == 'Mme'] <- 'Mrs'
combined_data$Title[combined_data$Title %in% rare_title] <- 'Rare Title'

# Now lets look at the table again
head(table(combined_data$Sex, combined_data$Title), 6)
head(table(combined_data$Title, combined_data$Sex), 10)


#Grab surname from the names
combined_data$Surname <- sapply(combined_data$Name, function(x) 
  strsplit(x, split = '[,.]')[[1]][1])

cat(paste('We have <b>', nlevels(factor(combined_data$Surname)), '</b> unique
          surnames. I would be interested to infer ethnicity based on surname --- another time.'))























glimpse(train_data)
glimpse(test_data)
