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

#First stage of data process: Missing Values removal
MissingValeus <- na.omit(data)


#Second stage of data process: Exclude 1-day developers
data2 = MissingValeus[!(MissingValeus$activity_period_in_days<=1),]

#Third stage of data process: Exclude short time contributors
data2 = data2[order(data2$activity_period_in_days,decreasing = TRUE),]
data3 = data2[-c(2307:2563),]
data3 = data3[,c('issues_participated',"issues_opened","issues_closed","commits_authored","violations_added","violations_eliminated", "average_time_to_close_issues", "issues_closed_per_day")]

#Fourth stage of data process: Data normalization 
normalized = as.data.frame(lapply(data3, function(x) (x - min(x))/(max(x) - min(x))))

#Metruc for Sepperation
tss = Distance(normalized)
plot(normalized)

#First Cluster experiement
normalized_euclidean = dist(normalized[1:4], method = "euclidean")
normalized_euclidean_hc_complete = hclust(normalized_euclidean, method = "complete")
normalized_euclidean_clusters = cutree(normalized_euclidean_hc_complete, k = 7)
plot(normalized, col = normalized_euclidean_clusters, pch = 15, main = "Single Linkage")


cluster_data = (normalized_euclidean_clusters == 2)
cluster_data = rbind((which(cluster_data == TRUE)))
deep_normalized = normalized[cluster_data,]
plot(deep_normalized)

#Second Cluater only in first experiementy's second cluster 
deep_normalized_euclidean = dist(deep_normalized[1:4], method = "euclidean")
deep_normalized_euclidean_hc_complete = hclust(deep_normalized_euclidean, method = "complete")
deep_normalized_euclidean_clusters = cutree(deep_normalized_euclidean_hc_complete, k = 7)

#New dataset that only contains Devs
dev_clusters = (normalized_euclidean_clusters == 1)
dev_clusters2 = (normalized_euclidean_clusters == 3)
dev_clusters3 = (deep_normalized_euclidean_clusters == 1)
dev_cluster_data = rbind(dev_clusters, dev_clusters2, dev_clusters3)
dev_cluster_data = rbind((which(dev_cluster_data == TRUE)))
dev_normalized = normalized[dev_cluster_data,]
plot(dev_normalized)
dev_normalized = na.omit(dev_normalized)

dev_normalized_euclidean = dist(dev_normalized[5:8], method = "euclidean")
dev_normalized_euclidean_hc_complete = hclust(dev_normalized_euclidean, method = "complete")

dev_normalized_euclidean_silhouette = c()
dev_normalized_euclidean_cohesion= c()
dev_normalized_euclidean_seperation= c()
for (i in 2:14){
  dev_normalized_euclidean_clusters = cutree(dev_normalized_euclidean_hc_complete, k = i)
  dev_normalized_euclidean_silhouette[i-1] = mean(silhouette(dev_normalized_euclidean_clusters, dev_normalized_euclidean)[, 3])
  dev_normalized_euclidean_cohesion[i-1] = wrap(i, dev_normalized_euclidean_hc_complete, dev_normalized[5:8])
  dev_normalized_euclidean_seperation[i-1] = tss - dev_normalized_euclidean_cohesion[i-1]  
  
}
 #Metrics for number of clusters decision
plot(dev_normalized_euclidean_silhouette, type = "b",pch=c(16), xlab="Number of Clusters", ylab="Mean Silhouette" )
plot(dev_normalized_euclidean_cohesion, type = "b",pch=c(16), xlab="Number of Clusters", ylab="Cohesion" )
plot(dev_normalized_euclidean_seperation, type = "b",pch=c(16), xlab="Number of Clusters", ylab="Separation" )

dev_normalized_euclidean_clusters = cutree(dev_normalized_euclidean_hc_complete, k = 6)
plot(dev_normalized[5:8], col = dev_normalized_euclidean_clusters, pch = 15, main = "Single Linkage")

boxplot(dev_normalized$violations_added ~ dev_normalized_euclidean_clusters, xlab='Cluster', ylab='violations_added')
boxplot(dev_normalized$violations_eliminated ~ dev_normalized_euclidean_clusters, xlab='Cluster', ylab='violations_eliminated')
boxplot(dev_normalized$average_time_to_close_issues ~ dev_normalized_euclidean_clusters, xlab='Cluster', ylab='average_time_to_close_issues')
boxplot(dev_normalized$issues_closed_per_day ~ dev_normalized_euclidean_clusters, xlab='Cluster', ylab='issues_closed_per_day')

