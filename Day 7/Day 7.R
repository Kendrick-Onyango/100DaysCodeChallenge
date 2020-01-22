
#Evaluating Scoring Models

rm(list = ls(all=TRUE))
cat('\014')


# Please have an up to date version of R (3.5.*, or newer)
# Answer "no" to:
# Do you want to install from sources the packages which need compilation?
update.packages(ask = FALSE, checkBuilt = TRUE)

pkgs <- c(
  "arules", "bitops", "caTools", "cdata", "data.table", "DBI",
  "dbplyr", "DiagrammeR", "dplyr", "e1071", "fpc", "ggplot2",
  "glmnet", "glmnetUtils", "gridExtra", "hexbin", "kernlab",
  "igraph", "knitr", "lime", "lubridate", "magrittr", "MASS",
  "mgcv", "pander", "plotly", "pwr", "randomForest", "readr",
  "readxl", "rmarkdown", "rpart", "rpart.plot", "RPostgres",
  "rqdatatable", "rquery", "RSQLite", "scales", "sigr", "sqldf",
  "tidypredict", "text2vec", "tidyr", "vtreat", "wrapr", "WVPlots",
  "xgboost", "xts", "webshot", "zeallot", "zoo")

install.packages(
  pkgs,
  dependencies = c("Depends", "Imports", "LinkingTo"))


#fit a regression model and make predictions
crickets <- read.csv('https://raw.githubusercontent.com/WinVector/PDSwR2/master/cricketchirps/crickets.csv', sep=',', header = T)
View(crickets)
str(crickets)

cricket_model <- lm(temperatureF ~ chirp_rate, data = crickets)
summary(cricket_model)
crickets$temp_pred <- predict(cricket_model, newdata = crickets)

#RMSE tells us by how much is the predicted temprature OFF??
error_sq <- (crickets$temp_pred - crickets$temperatureF)^2
(RMSE <- sqrt(mean(error_sq)))



#double density plots - evaluating probability models
#install rquery and wrapr first (download from CRAN and install locally)
install.packages("WVPlots")
library(WVPlots)

DoubleDensityPlot(spamTest,
                  xvar = 'pred',
                  truthVar = 'spam',
                  title = 'Distribution of scores for spam filter')

#Receiver Operating Characteristic curve
ROCPlot(spamTest,
        xvar = 'pred',
        truthVar = 'spam',
        truthTarget = 'spam',
        title = 'spam filter test performance')

library(sigr)
calcAUC(spamTest$pred, spamTest$spam == 'spam')



#call in Day 5 work first to perform the steps above
spamD <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data', sep = ',', header = FALSE)

spamCols <- c(
  'word.freq.make', 'word.freq.address', 'word.freq.all',
  'word.freq.3d', 'word.freq.our', 'word.freq.over', 'word.freq.remove',
  'word.freq.internet', 'word.freq.order', 'word.freq.mail',
  'word.freq.receive', 'word.freq.will', 'word.freq.people',
  'word.freq.report', 'word.freq.addresses', 'word.freq.free',
  'word.freq.business', 'word.freq.email', 'word.freq.you',
  'word.freq.credit', 'word.freq.your', 'word.freq.font',
  'word.freq.000', 'word.freq.money', 'word.freq.hp', 'word.freq.hpl',
  'word.freq.george', 'word.freq.650', 'word.freq.lab',
  'word.freq.labs', 'word.freq.telnet', 'word.freq.857',
  'word.freq.data', 'word.freq.415', 'word.freq.85',
  'word.freq.technology', 'word.freq.1999', 'word.freq.parts',
  'word.freq.pm', 'word.freq.direct', 'word.freq.cs',
  'word.freq.meeting', 'word.freq.original', 'word.freq.project',
  'word.freq.re', 'word.freq.edu', 'word.freq.table',
  'word.freq.conference', 'char.freq.semi', 'char.freq.lparen',
  'char.freq.lbrack', 'char.freq.bang', 'char.freq.dollar',
  'char.freq.hash', 'capital.run.length.average',
  'capital.run.length.longest', 'capital.run.length.total',
  'spam'
)

colnames(spamD) <- spamCols
spamD$spam <- as.factor(ifelse(spamD$spam>0.5, 'spam', 'non-spam'))
set.seed(18012020)
spamD$rgroup <- floor(100*runif(dim(spamD)[[1]]))
write.table(spamD, file='spamD.tsv',quote = F, sep = '\t', row.names = F)




#### Classification problems - Multicategory | Two-category Classification
#read data into R 
spamD <- read.table('spamD.tsv', header= TRUE, sep = '\t')


#partion data into training and test datasets
spamTrain <- subset(spamD,spamD$rgroup >= 10)
spamTest <- subset(spamD, spamD$rgroup < 10)


#Create a formula that describes the model 
spamVars <- setdiff(colnames(spamD), list('rgroup','spam'))

spamFormula <- as.formula(paste('spam == "spam"',
                                paste(spamVars, collapse = '+'), sep = '~'))


#Fit the logistic regression model 
spamModel <- glm(spamFormula, family = binomial(link = 'logit'),
                 data = spamTrain, maxit = 100)

#Make predictions on the training and test sets 
spamTrain$pred <- predict(spamModel, newdata = spamTrain,
                          type = 'response')
spamTest$pred <- predict(spamModel, newdata = spamTest,
                         type = 'response')





