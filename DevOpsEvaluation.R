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
data3 = data3[,c('issues_participated',"issues_opened","issues_closed","commits_authored","violations_added","violations_eliminated", "average_time_to_close_issues", "average_issues_comments_length", "average_comments_per_issue")]

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
devops_clusters = (normalized_euclidean_clusters == 4)
devops_clusters2 = (normalized_euclidean_clusters == 5)
devops_clusters3 = (deep_normalized_euclidean_clusters == 5)
devops_clusters4 = (deep_normalized_euclidean_clusters == 6)
devops_clusters5 = (deep_normalized_euclidean_clusters == 7)
devops_cluster_data = rbind(devops_clusters, devops_clusters2, devops_clusters3, devops_clusters4, devops_clusters5)
devops_cluster_data = rbind((which(devops_cluster_data == TRUE)))
devops_normalized = normalized[devops_cluster_data,]
plot(devops_normalized)
devops_normalized = na.omit(devops_normalized)

devops_normalized_euclidean = dist(devops_normalized[5:9], method = "euclidean")
devops_normalized_euclidean_hc_complete = hclust(devops_normalized_euclidean, method = "complete")

devops_normalized_euclidean_silhouette = c()
devops_normalized_euclidean_cohesion= c()
devops_normalized_euclidean_seperation= c()
for (i in 2:14){
  devops_normalized_euclidean_clusters = cutree(devops_normalized_euclidean_hc_complete, k = i)
  devops_normalized_euclidean_silhouette[i-1] = mean(silhouette(devops_normalized_euclidean_clusters, devops_normalized_euclidean)[, 3])
  devops_normalized_euclidean_cohesion[i-1] = wrap(i, devops_normalized_euclidean_hc_complete, devops_normalized[5:9])
  devops_normalized_euclidean_seperation[i-1] = tss - devops_normalized_euclidean_cohesion[i-1]  
  
}
#Metrics for number of clusters decision
plot(devops_normalized_euclidean_silhouette, type = "b",pch=c(16), xlab="Number of Clusters", ylab="Mean Silhouette" )
plot(devops_normalized_euclidean_cohesion, type = "b",pch=c(16), xlab="Number of Clusters", ylab="Cohesion" )
plot(devops_normalized_euclidean_seperation, type = "b",pch=c(16), xlab="Number of Clusters", ylab="Separation" )

devops_normalized_euclidean_clusters = cutree(devops_normalized_euclidean_hc_complete, k = 8)
plot(devops_normalized[5:9], col = devops_normalized_euclidean_clusters, pch = 15, main = "Single Linkage")

boxplot(devops_normalized$violations_added ~ devops_normalized_euclidean_clusters, xlab='Cluster', ylab='violations_added')
boxplot(devops_normalized$violations_eliminated ~ devops_normalized_euclidean_clusters, xlab='Cluster', ylab='violations_eliminated')
boxplot(devops_normalized$average_time_to_close_issues ~ devops_normalized_euclidean_clusters, xlab='Cluster', ylab='average_time_to_close_issues')
boxplot(devops_normalized$average_comments_per_issue ~ devops_normalized_euclidean_clusters, xlab='Cluster', ylab='average_comments_per_issue')
boxplot(devops_normalized$average_issues_comments_length ~ devops_normalized_euclidean_clusters, xlab='Cluster', ylab='average_issues_comments_length')

