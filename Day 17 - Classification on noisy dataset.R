
#=====================================================================================================
####  Machine Learning -- Classification On a Noisy Dataset
#=====================================================================================================

#House Keeping
rm(list = ls(all=TRUE))
cat('\014')


#set working directory
path <- 'C:/Users/mwm9/Desktop/100DaysOfCode/100DaysCodeChallenge/Day 17'
setwd(path)
list.files()


#read datasets from UCI Machine Learning Database
training_data <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/00421/aps_failure_training_set.csv', skip = 20, na.strings = 'na')
test_data <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/00421/aps_failure_test_set.csv', skip = 20, na.strings = 'na')


#check the dimensions
dim(training_data)
dim(test_data)

#install unistalled packages
pkgs <- c('dplyr', 'caret', 'caretEnsemble', 'mice', 'doParallel', 'car')

lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

#load all 
lapply(pkgs, library, character.only =TRUE)



# #load packages
# library(dplyr)
# library(caret)
# library(caretEnsemble)
# library(mice)
# library(doParallel)
# library(car)



#Exploratory Data Analysis

glimpse(training_data)
glimpse(test_data)

View(training_data)
View(test_data)


#Focus on the Target variable -- Class (neg and pos)
summary(training_data$class)
summary(test_data$class)


#check proportions
options(digits = 4)
prop.table(table(training_data$class))
prop.table(table(test_data$class))
#class totally imbalanced ~ deal with this later when addressing accuracy of the Model




#Look at the summary table for other remaining features - also calculates the mean quartiles for all the data and number of missing values
options(scipen = 999)

summary_df <- do.call(cbind, lapply(training_data[, 2:ncol(training_data)], summary))
summary_df_table <- as.data.frame(round(t(summary_df),0))

names(summary_df_table)[7] <- paste('Missing_values')


summary_df_table_2 <- summary_df_table %>% 
  mutate(obs = nrow(training_data),
         Missing_prop = Missing_values / obs)

print(summary_df_table_2)
#Each row is a feature


summary_df_table_2 %>% summarise(Min = mean(Min.),
                                 first_Q = mean(`1st Qu.`),
                                 Median = median(Median),
                                 Mean = mean(Mean),
                                 third_Q = mean(`3rd Qu.`),
                                 Max = max(Max.),
                                 mean_MV = mean(Missing_values),
                                 obs = mean(obs),
                                 mean_MV_perc = mean_MV / obs)

#from this we can see that an average of 5000 out of 60000 is missing (8.3% Missing) -- Training Dataset


##### Summary for the Test data #######

summary_df1 <- do.call(cbind, lapply(test_data[, 2:ncol(test_data)], summary))
summary_df1_table <- as.data.frame(round(t(summary_df1),0))

names(summary_df1_table)[7] <- paste('Missing_values')


summary_df1_table_2 <- summary_df1_table %>% 
  mutate(obs = nrow(test_data),
         Missing_prop = Missing_values / obs)

print(summary_df1_table_2)
#Each row is a feature


summary_df1_table_2 %>% summarise(Min = mean(Min.),
                                 first_Q = mean(`1st Qu.`),
                                 Median = median(Median),
                                 Mean = mean(Mean),
                                 third_Q = mean(`3rd Qu.`),
                                 Max = max(Max.),
                                 mean_MV = mean(Missing_values),
                                 obs = mean(obs),
                                 mean_MV_perc = mean_MV / obs)


#from this we can see that an average of 1345 out of 16000 is missing (8.4% Missing) -- Test Dataset





####### A lot of missing values - Start Imputation -- But first combine the whole data, impute and then split again (We want to use Full Information)####
#replicate our sets
training_data_bind <- training_data
test_data_bind <- test_data

#create a new column 'set' to label observations
training_data_bind$set <- 'TRAIN'
test_data_bind$set <- 'TEST'


#Merge them into 1 single set
full_dataset <- rbind(training_data_bind, test_data_bind)
dim(full_dataset)

set.seed(123)
imputed_full <- mice(full_dataset, m = 1, maxit = 5, method = 'mean', seed = 500)


full_imputed <- complete(imputed_full, 1)


#check that we still maintain the same dimensions
dim(full_imputed)

(na_count_full_imputed <- data.frame(sapply(full_imputed,function(y) sum(length(which(is.na(y)))) )))


#there are still some features with missing values
issue_columns <- subset(imputed_full$loggedEvents, meth == 'constant' | meth == 'collinear')
print(issue_columns)


#if the feature are collinear we may want to drop them
#create vector of column names

issue_columns_names <- as.character(issue_columns[, 'out'])
issue_columns_names <- issue_columns_names[-2]
print(issue_columns_names)


#remove these columns and store final dataset
full_imputed_filtered <- full_imputed[, !(names(full_imputed) %in% issue_columns_names)]

dim(full_imputed_filtered)


#solved our initial missing values problem



#Now split the dataset into training and testing sets again
#subset the full_imputed_filteres data

training_data_imputed <- subset(full_imputed_filtered, set == 'TRAIN')
test_data_imputed <- subset(full_imputed_filtered, set == 'TEST')


#drop the 'set' column as we don't need it anymore
training_data_imputed$set <- NULL
test_data_imputed$set <- NULL


#check dimensions
dim(training_data_imputed)
dim(test_data_imputed)


#check for outliers and other inconsistent data points: Box-plots
cooksd <- cooks.distance(glm(class ~ .,family = 'binomial', data = training_data_imputed))



plot(cooksd,pch="*",cex=2,main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")
#thick black line are all the data points bunched together

#Notice a point outside this...could be a bunch of data points - OUTLIERS
outliers <- rownames(training_data_imputed[cooksd > 4*mean(cooksd, na.rm = TRUE), ])
print(outliers)
#only 22 data points can be considered outliers according to the cook's distance test


#check for multicollinearity
sum((correlation > 0.5 | correlation < -0.5) & correlation < 1) / (162*162)
sum((correlation > 0.7 | correlation < -0.7) & correlation < 1) / (162*162)
sum((correlation > 0.9 | correlation < -0.9) & correlation < 1) / (162*162)
#only 12% have a correlation score higher than 0.5
#only 4.7% have a correlation higher than 0.7
#only 4.7% have a correlation higher than 0.9

## Multicollinearity not a problem here

#################### Feature Engineering ############################
## to be done in a different exercise

#### Modelling 

# Now data set for modelling --- we use logistic regression model and Naive Bayes model.
# Both are fast compared to more advanced algorithms such as random forests, SVMs and GBM

# Use `caret` and `caretEnsemble` and `caretList()` in caretEnsemble to train both models with the same resampling

registerDoParallel(3)
getDoParWorkers()

set.seed(123)

my_ctrl <- trainControl(method = 'cv', 
                        number = 5,
                        classProbs = TRUE,
                        savePredictions = 'final',
                        index = createResample(training_data_imputed$class, 3),
                        sampling = 'up',
                        allowParallel = TRUE)


model_list <- caretList(class ~ ., 
                        data = training_data_imputed,
                        methodList = c('glm', 'nb'),
                        tuneList = NULL,
                        continue_on_fail = FALSE,
                        preProcess = c('center', 'scale'),
                        trControl = my_ctrl)



### Performace on unseen (Test) data
#Logistic Regression model
confusionMatrix(predict(model_list$glm,test_data_imp, type = "raw"), test_data_imp$class)

#Naive Bayes model
confusionMatrix(predict(model_list$nb,test_data_imp, type = "raw"), test_data_imp$class)

































