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
data3 = data3[,c('issues_participated',"issues_opened","issues_closed","commits_authored", "average_issues_comments_length", "average_comments_per_issue", "inactive_period_within_active_period", "issues_opened")]

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
ops_clusters = (normalized_euclidean_clusters == 6)
ops_clusters2 = (deep_normalized_euclidean_clusters == 3)
ops_clusters3 = (deep_normalized_euclidean_clusters == 4)
ops_cluster_data = rbind(ops_clusters, ops_clusters2, ops_clusters3)
ops_cluster_data = rbind((which(ops_cluster_data == TRUE)))
ops_normalized = normalized[ops_cluster_data,]
plot(ops_normalized)
ops_normalized = na.omit(ops_normalized)

ops_normalized_euclidean = dist(ops_normalized[5:8], method = "euclidean")
ops_normalized_euclidean_hc_complete = hclust(ops_normalized_euclidean, method = "complete")

ops_normalized_euclidean_silhouette = c()
ops_normalized_euclidean_cohesion= c()
ops_normalized_euclidean_seperation= c()
for (i in 2:14){
  ops_normalized_euclidean_clusters = cutree(ops_normalized_euclidean_hc_complete, k = i)
  ops_normalized_euclidean_silhouette[i-1] = mean(silhouette(ops_normalized_euclidean_clusters, ops_normalized_euclidean)[, 3])
  ops_normalized_euclidean_cohesion[i-1] = wrap(i, ops_normalized_euclidean_hc_complete, ops_normalized[5:8])
  ops_normalized_euclidean_seperation[i-1] = tss - ops_normalized_euclidean_cohesion[i-1]  
  
}
#Metrics for number of clusters decision
plot(ops_normalized_euclidean_silhouette, type = "b",pch=c(16), xlab="Number of Clusters", ylab="Mean Silhouette" )
plot(ops_normalized_euclidean_cohesion, type = "b",pch=c(16), xlab="Number of Clusters", ylab="Cohesion" )
plot(ops_normalized_euclidean_seperation, type = "b",pch=c(16), xlab="Number of Clusters", ylab="Separation" )

ops_normalized_euclidean_clusters = cutree(ops_normalized_euclidean_hc_complete, k = 4)
plot(ops_normalized[5:8], col = ops_normalized_euclidean_clusters, pch = 15, main = "Single Linkage")

boxplot(ops_normalized$inactive_period_within_active_period ~ ops_normalized_euclidean_clusters, xlab='Cluster', ylab='inactive_period_within_active_period')
boxplot(ops_normalized$average_comments_per_issue ~ ops_normalized_euclidean_clusters, xlab='Cluster', ylab='average_comments_per_issue')
boxplot(ops_normalized$average_issues_comments_length ~ ops_normalized_euclidean_clusters, xlab='Cluster', ylab='average_issues_comments_length')
boxplot(ops_normalized$issues_opened ~ ops_normalized_euclidean_clusters, xlab='Cluster', ylab='issues_opened')

