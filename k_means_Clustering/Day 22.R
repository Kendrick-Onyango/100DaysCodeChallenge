#============================================================================
# Clustering analysis : K-means and Hierarchical Clustering
#============================================================================

#House keeping and setting working directory
rm(list = ls(all=TRUE))
cat('\014')

path <- 'C:/Users/Mustapha/Desktop/100DaysofCode/100DaysCodeChallenge/k_means_Clustering'

setwd(path)
list.files()


#libraries
library(tidyverse)


#load data 

Eujobs <- read.csv('Eurojobs.csv', header = TRUE, dec = '.',sep = ',', row.names = 1)


#change to a tibble dataset and look at the variables
Eujobs <- tbl_df(Eujobs)
head(Eujobs)
glimpse(Eujobs)

names(Eujobs)

Eujobs %>% na.omit() 
  

# kmeans clustering
model <- kmeans(Eujobs, centers = 2)
print(model$cluster)


# Bind with clusters
EU_data <- Eujobs %>%
    mutate(clusters = model$cluster)

Eujobs_raw <- read.csv('Eurojobs.csv', header = TRUE, dec = '.',sep = ',')
EU <- Eujobs_raw %>% 
      mutate(clusters = model$cluster)
names(EU)
View(EU)

EU_cluster <- data.frame(Eujobs_raw, cluster = as.factor(model$cluster))
glimpse(EU_cluster)



#model with different centres
model2 <- kmeans(Eujobs, centers = 4)
print(model2$cluster)
model2$betweenss
model2$withinss
model2$totss

# kmeans statistics for model 1
model$betweenss
model$withinss
model$totss

# model3
model3 <- kmeans(Eujobs, centers = 8)
print(model3$cluster)
model3$betweenss
model3$withinss
model3$totss



#quality of partition
quality <- (model$betweenss)/(model$totss)
quality2 <- (model2$betweenss)/(model2$totss)
quality3 <- (model3$betweenss)/(model3$totss)


#===========================================================================
# Hierarchical Clustering
#===========================================================================
# Number of classes not specified in advance. H.Clustering helps to determine the optimal number of clusters


# # It starts by putting every point in its own cluster, 
# so each cluster is a singleton
# # It then merges the 2 points that are closest to each 
# other based on the distances from the distance matrix. 
# The consequence is that there is one less cluster
# # It then recalculates the distances between the new and 
# old clusters and save them in a new distance matrix which 
# will be used in the next step
# # Finally, steps 1 and 2 are repeated until all 
# clusters are merged into one single cluster including all points.
# # 

### Methods Under Hierachical Clustering
# Single linkage: computes the minimum distance between clusters before merging them.
# Complete linkage: computes the maximum distance between clusters before merging them.
# Average linkage: computes the average distance between clusters before merging them.
# Centroid linkage: calculates centroids for both clusters, then computes the distance between the two before merging them.

h_clusters <- hclust(dist(Eujobs[,1:9]))
plot(h_clusters)

#From plot number of optimal clusters could be 3 or 4

cluster_cut <- cutree(h_clusters, 4)

# Look at the clustering by checking the country groups
table(cluster_cut,Eujobs_raw$Country)


#Using Average linkage
h_clusters1 <- hclust(dist(Eujobs[,1:9]), method = 'average')
plot(h_clusters1)

#Can still do with 3 or 4 clusters
cluster_cut1 <- cutree(h_clusters1, 3)
table(cluster_cut1,Eujobs_raw$Country)

ggplot(Eujobs_raw, aes(SPS, Min, color = Eujobs_raw$Country)) +
  geom_point(alpha = 0.4, size = 3.5) + 
  geom_point(col = cluster_cut) 



#Using Average linkage
h_clusters2 <- hclust(dist(Eujobs[,1:9]), method = 'centroid')
plot(h_clusters2)








