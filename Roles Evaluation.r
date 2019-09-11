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
plot(original)

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
plot(normalized)








#DBSCAN
library(dbscan)
model = dbscan(normalized, eps = 0.01, minPts = 4)
plot(normalized, col = model$cluster , xlim=c(0, 0.2), ylim=c(0,0.2))
clut = (model$cluster == 1)
yo = rbind(which(clut == TRUE))
no_irregulars = normalized[yo,]
plot(no_irregulars)









#K means
original_mean_silhouette_array = c()
original_cohesion_array = c()
original_separation_array = c()
normalized_mean_silhouette_array = c()
normalized_cohesion_array = c()
normalized_separation_array = c()
no_irregulars_mean_silhouette_array = c()
no_irregulars_cohesion_array = c()
no_irregulars_separation_array = c()
#Test for multiple pcs
for (k in 2:14) {
  #Loop for Original data
  kmeans_original = kmeans(original, centers = k)
  original_cohesion = kmeans_original$tot.withinss
  original_cohesion_array[k] = original_cohesion
  original_separation = kmeans_original$betweenss
  original_separation_array[k] = original_separation
  
  original_model_silhouette = silhouette(kmeans_original$cluster, dist(original))
  original_mean_silhouette = mean(original_model_silhouette[, 3])
  original_mean_silhouette_array[k] <- original_mean_silhouette
  
  #Loop for Normalized data
  kmeans_normalized = kmeans(normalized, centers = k)
  normalized_cohesion = kmeans_normalized$tot.withinss
  normalized_cohesion_array[k] = normalized_cohesion
  normalized_separation = kmeans_normalized$betweenss
  normalized_separation_array[k] = normalized_separation
  
  normalized_model_silhouette = silhouette(kmeans_normalized$cluster, dist(normalized))
  normalized_mean_silhouette = mean(normalized_model_silhouette[, 3])
  normalized_mean_silhouette_array[k]<- normalized_mean_silhouette
  
  #Loop for No Irregulars data
  kmeans_no_irregulars = kmeans(no_irregulars, centers = k)
  no_irregulars_cohesion = kmeans_no_irregulars$tot.withinss
  no_irregulars_cohesion_array[k] = no_irregulars_cohesion
  no_irregulars_separation = kmeans_no_irregulars$betweenss
  no_irregulars_separation_array[k] = no_irregulars_separation
  
  no_irregulars_model_silhouette = silhouette(kmeans_no_irregulars$cluster, dist(no_irregulars))
  no_irregulars_mean_silhouette = mean(no_irregulars_model_silhouette[, 3])
  no_irregulars_mean_silhouette_array[k]<- no_irregulars_mean_silhouette
}

# Mean Silhouette 
plot(no_irregulars_mean_silhouette_array, col = 'red', type = "b",pch=c(16), xlab="Number of Clusters", ylab="Mean Silhouette" , ylim = c(0.4,1))
legend(x=2,y=0.55,c("no_irregulars","original","normalized"),cex=1, col=c("red","blue","green"),pch=c(16,17,15), lty=c(1,1,1))
lines(original_mean_silhouette_array, col = 'blue', type = "b", pch=c(17), xlab="Number of Clusters", ylab="Mean Silhouette")
lines(normalized_mean_silhouette_array, col = 'green', type = "b", pch=c(15), xlab="Number of Clusters", ylab="Mean Silhouette")

#Cohesion
plot(no_irregulars_cohesion_array, col = 'red', type = "b",pch=c(16), xlab="Number of Clusters", ylab="Cohesion", ylim = c(0,4) )
legend(x=2,y=0.55,c("no_irregulars",),cex=1, col=c("red","blue","green"),pch=c(16,17,15), lty=c(1,1,1))

plot(original_cohesion_array, col = 'blue', type = "b", pch=c(17), xlab="Number of Clusters", ylab="Cohesion")
lines(normalized_cohesion_array, col = 'green', type = "b", pch=c(15))

#Seperation
plot(original_separation_array, col = 'red', type = "b",pch=c(16), xlab="Number of Clusters", ylab="Seperation" )
legend(x=10,y=8,c("no_irregulars","original","normalized"),cex=1, col=c("red","blue","green"),pch=c(16,17,15), lty=c(1,1,1))
lines(original_separation_array, col = 'blue', type = "b", pch=c(17))
lines(normalized_separation_array, col = 'green', type = "b", pch=c(15))


kmeans_normalized = kmeans(normalized, centers = 6)
cohesion = kmeans_normalized$tot.withinss
separation = kmeans_normalized$betweenss
model_silhouette = silhouette(kmeans_normalized$cluster, dist(normalized))
mean_silhouette = mean(model_silhouette[, 3])
plot(normalized, col = kmeans_normalized$cluster)
boxplot(normalized$issues_participated ~ kmeans_normalized$cluster, xlab='Cluster', ylab='issues_participated')
boxplot(normalized$issues_opened ~ kmeans_normalized$cluster, xlab='Cluster', ylab='issues_opened')
boxplot(normalized$issues_closed ~ kmeans_normalized$cluster, xlab='Cluster', ylab='issues_closed')
boxplot(normalized$commits_authored ~ kmeans_normalized$cluster, xlab='Cluster', ylab='commits_authored')












#Hierchical Clustering Complete


#Euclidean distance
no_irregulars_euclidean = dist(no_irregulars, method = "euclidean")
no_irregulars_euclidean_hc_complete = hclust(no_irregulars_euclidean, method = "complete")

normalized_euclidean = dist(normalized, method = "euclidean")
normalized_euclidean_hc_complete = hclust(normalized_euclidean, method = "complete")

tss = Distance(normalized)


normalized_euclidean_silhouette = c()
normalized_euclidean_cohesion = c()
normalized_euclidean_seperation = c()
no_irregulars_euclidean_silhouette = c()
no_irregulars_euclidean_cohesion = c()
no_irregulars_euclidean_seperation = c()

for (i in 2:20){
  #Normalized dataset
  
  normalized_euclidean_clusters = cutree(normalized_euclidean_hc_complete, k = i)
  normalized_euclidean_silhouette[i-1] = mean(silhouette(normalized_euclidean_clusters, normalized_euclidean)[, 3])
  normalized_euclidean_cohesion[i-1] = wrap(i, normalized_euclidean_hc_complete, normalized)
  normalized_euclidean_seperation[i-1] = tss - normalized_euclidean_cohesion[i-1]  
  
  #No Irregulars dataset
  
  no_irregulars_euclidean_clusters = cutree(no_irregulars_euclidean_hc_complete, k = i)
  no_irregulars_euclidean_silhouette[i-1] = mean(silhouette(no_irregulars_euclidean_clusters, no_irregulars_euclidean)[, 3])
  no_irregulars_euclidean_cohesion[i-1] = wrap(i, no_irregulars_euclidean_hc_complete, no_irregulars)
  no_irregulars_euclidean_seperation[i-1] = tss - no_irregulars_euclidean_cohesion[i-1]  
  

}

#Cohesion plot
plot(no_irregulars_euclidean_cohesion, col = 'red', pch=c(16), type = "b", xlab="Number of Clusters", ylab="Cohesion", ylim = c(0,4))
lines(normalized_euclidean_cohesion, col = 'green', pch=c(15), type = "b", xlab="Number of Clusters", ylab="Cohesion")
legend(x=11.5,y=4,c("no_irregulars_euclidean","normalized_euclidean"),cex=1, col=c("red","green"),pch=c(16,15), lty=c(1,1))


#Seperation plot
plot(no_irregulars_euclidean_seperation, col = 'red', pch=c(16), type = "b", xlab="Number of Clusters", ylab="Seperation")
lines(normalized_euclidean_seperation, col = 'green', pch=c(15), type = "b", xlab="Number of Clusters", ylab="Seperation")
legend(x=11.5,y=6,c("no_irregulars_euclidean","normalized_euclidean"),cex=1, col=c("red","green"),pch=c(16,15), lty=c(1,1))

#Silhouette plot
plot(no_irregulars_euclidean_silhouette, col = 'red', pch=c(16), type = "b", xlab="Number of Clusters", ylab="Silhouette", ylim = c(0.5,1))
lines(normalized_euclidean_silhouette, col = 'green', pch=c(15), type = "b", xlab="Number of Clusters", ylab="Silhouette")
legend(x=11.5,y=1,c("no_irregulars_euclidean","normalized_euclidean"),cex=1, col=c("red","green"),pch=c(16,15), lty=c(1,1))


#Results
normalized_euclidean_clusters = cutree(normalized_euclidean_hc_complete, k = 7)
plot(normalized, col = normalized_euclidean_clusters, pch = 15, main = "Single Linkage")


boxplot(normalized$issues_participated ~ normalized_euclidean_clusters, xlab='Cluster', ylab='issues_participated')
boxplot(normalized$issues_opened ~ normalized_euclidean_clusters, xlab='Cluster', ylab='issues_opened')
boxplot(normalized$issues_closed ~ normalized_euclidean_clusters, xlab='Cluster', ylab='issues_closed')
boxplot(normalized$commits_authored ~ normalized_euclidean_clusters, xlab='Cluster', ylab='commits_authored')





#Deep Role Classification

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


