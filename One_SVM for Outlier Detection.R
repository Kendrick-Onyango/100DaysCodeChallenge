
# Importing Dataset
# Demonstrate Kernel Density Estimation - method to detect outliers/anomalies
rm(list = ls(all=TRUE))
cat('\014')


# load library and data
library(MASS)
iris_data <- iris
View(iris_data)


#convert dataset with 4 independent variables into matrix to run density function
iris_mat <- as.matrix(iris_data[,1:4])

#Running the density function
kde <- density(iris_mat)


# Densities are given by the y attribute of kde
# convert the output of densities to a dataframe
densities <- as.data.frame(kde$y)



# Compute the benchmark for outlier detection
# compare the minimum density with the mean of density and flagged obs falling below this benchmark
min_density <- min(densities$`kde$y`)
mean_density <- mean(densities$`kde$y`)
bench <- min_density/mean_density




# make the outliers using the benchmark
densities$outlier <- ifelse(densities$`kde$y`<bench, 1,0)
densities1 <- densities
densities1$outlier <- as.factor(densities1$outlier)
levels(densities1$outlier)



# Summary of the Outliers
summary(densities1$outlier)





# ONE Class SVM
# Is a unsupervised algorithm that learns a decision function to identify outliers
library(e1071)

#Lets work with the Iris dataset
iris_X <- iris_data[,1:4]


# Building and Fitting Model
# ny defines the upper bound of the fraction of outliers
# initialize the model and fit it on iris dataset
model_oneclasssvm <- svm(iris_X, type = 'one-classification',kernel = 'radial',
                         gamma = 0.05,nu = 0.05)
model_oneclasssvm


#defining outliers
pred_oneclasssvm <- predict(model_oneclasssvm, iris_X)
pred_oneclasssvm


#computing summary stats
summary(pred_oneclasssvm)

# All the values labelled False are outliers













