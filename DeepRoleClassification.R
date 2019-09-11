library(cluster)
library(class)
library(MLmetrics)
library(dplyr)
library(tidyr)
library(ggplot2)


Distance <- function(cluster)
{
  # the center of the cluster, mean of all the points
  center <- colMeans(cluster)
  
  # calculate the summed squared error between every point and 
  # the center of that cluster 
  distance <- apply( cluster, 1, function(row)
  {
    sum( ( row - center )^2 )
  }) %>% sum()
  
  return(distance)
}


wss <- function(d) {
  sum(scale(d, scale = FALSE)^2)
}

wrap <- function(i, hc, x) {
  cl <- cutree(hc, i)
  spl <- split(x, cl)
  wss <- sum(sapply(spl, wss))
  wss
}


dataset = read.csv("dataset.csv", sep=';', header = TRUE, na.strings = c("NA"), colClasses = c("character", "character", "numeric", "numeric", "integer", "integer", "integer", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer", "numeric", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer"))

data= as.data.frame(dataset)

#Original dataset
original = data[,c('issues_participated',"issues_opened","issues_closed","commits_authored")]
original = na.omit(original)

#First stage of data process: Missing Values removal
MissingValeus <- na.omit(data)


#Second stage of data process: Exclude 1-day developers
data2 = MissingValeus[!(MissingValeus$activity_period_in_days<=1),]

#Third stage of data process: Exclude short time contributors
data2 = data2[order(data2$activity_period_in_days,decreasing = TRUE),]
data3 = data2[-c(2307:2563),]
data3 = data3[,c('issues_participated',"issues_opened","issues_closed","commits_authored")]

#Fourth stage of data process: Data normalization 
normalized = as.data.frame(lapply(data3, function(x) (x - min(x))/(max(x) - min(x))))
tss = Distance(normalized)
plot(normalized)

#Euclidean distance
normalized_euclidean = dist(normalized, method = "euclidean")
normalized_euclidean_hc_complete = hclust(normalized_euclidean, method = "complete")
normalized_euclidean_clusters = cutree(normalized_euclidean_hc_complete, k = 7)
plot(normalized, col = normalized_euclidean_clusters, pch = 15, main = "Single Linkage")

boxplot(normalized$issues_participated ~ normalized_euclidean_clusters, xlab='Cluster', ylab='issues_participated')
boxplot(normalized$issues_opened ~ normalized_euclidean_clusters, xlab='Cluster', ylab='issues_opened')
boxplot(normalized$issues_closed ~ normalized_euclidean_clusters, xlab='Cluster', ylab='issues_closed')
boxplot(normalized$commits_authored ~ normalized_euclidean_clusters, xlab='Cluster', ylab='commits_authored')

cluster_data = (normalized_euclidean_clusters == 2)
cluster_data = rbind((which(cluster_data == TRUE)))
deep_normalized = normalized[cluster_data,]
plot(deep_normalized)

deep_normalized_euclidean = dist(deep_normalized, method = "euclidean")
deep_normalized_euclidean_hc_complete = hclust(deep_normalized_euclidean, method = "complete")

deep_normalized_euclidean_silhouette = c()
deep_normalized_euclidean_cohesion= c()
deep_normalized_euclidean_seperation= c()
for (i in 2:14){
  deep_normalized_euclidean_clusters = cutree(deep_normalized_euclidean_hc_complete, k = i)
  deep_normalized_euclidean_silhouette[i-1] = mean(silhouette(deep_normalized_euclidean_clusters, deep_normalized_euclidean)[, 3])
  deep_normalized_euclidean_cohesion[i-1] = wrap(i, deep_normalized_euclidean_hc_complete, deep_normalized)
  deep_normalized_euclidean_seperation[i-1] = tss - deep_normalized_euclidean_cohesion[i-1]  
  
}

#Cohesion plot
plot(deep_normalized_euclidean_cohesion, type = "b", pch = 16, xlab="Number of Clusters", ylab="Cohesion")

#Seperation plot
plot(deep_normalized_euclidean_seperation, type = "b", pch = 16, xlab="Number of Clusters", ylab="Seperation")

#Silhouette plot
plot(deep_normalized_euclidean_silhouette, type = "b", pch = 16, xlab="Number of Clusters", ylab="Silhouette")

deep_normalized_euclidean_clusters = cutree(deep_normalized_euclidean_hc_complete, k = 7)
plot(deep_normalized, col = deep_normalized_euclidean_clusters, pch = 15, main = "Single Linkage")


boxplot(deep_normalized$issues_participated ~ deep_normalized_euclidean_clusters, xlab='Cluster', ylab='issues_participated')
boxplot(deep_normalized$issues_opened ~ deep_normalized_euclidean_clusters, xlab='Cluster', ylab='issues_opened')
boxplot(deep_normalized$issues_closed ~ deep_normalized_euclidean_clusters, xlab='Cluster', ylab='issues_closed')
boxplot(deep_normalized$commits_authored ~ deep_normalized_euclidean_clusters, xlab='Cluster', ylab='commits_authored')


