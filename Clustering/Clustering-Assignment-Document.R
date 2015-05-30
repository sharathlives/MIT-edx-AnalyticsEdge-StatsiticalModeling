#load the dataset
dailykos <- read.csv("dailykos.csv")

#Hierarchical Clustering
distances <- dist(dailykos, method = "euclidean")
clusterDailyKos = hclust(distances, method = "ward.D")
plot(clusterDailyKos) # The choices 2 and 3 are good cluster choices according to the dendrogram, because there is a lot of space between the horizontal lines in the dendrogram in those cut off spots (draw a horizontal line across the dendrogram where it crosses 2 or 3 vertical lines).

#we are trying to cluster news articles or blog posts into groups. This can be used to show readers categories to choose from when trying to decide what to read. 
#Thinking about the application, it is probably better to show the reader more categories than 2 or 3. These categories would probably be too broad to be useful. Seven or eight categories seems more reasonable.
hierGroups = cutree(clusterDailyKos, k = 7)

#Find number of observations in each cluster
rm(split) #remove variable called split in the workspace
HierCluster = split(dailykos, hierGroups) #spit the dailykos daatset based on hierGroups 
table(hierGroups)

#look at the top 5 words in each cluster
tail(sort(colMeans(HierCluster[[1]])))
tail(sort(colMeans(HierCluster[[2]])))
tail(sort(colMeans(HierCluster[[3]])))
tail(sort(colMeans(HierCluster[[4]])))
tail(sort(colMeans(HierCluster[[5]])))
tail(sort(colMeans(HierCluster[[6]])))
tail(sort(colMeans(HierCluster[[7]])))


#Use k-means clusters
set.seed(1000)
KmeansCluster = kmeans(dailykos, centers=7)
KmeansCluster = split(dailykos, KmeansCluster$cluster)
table(KmeansCluster)

#Output the most frequent words in each cluster
tail(sort(colMeans(KmeansCluster[[1]])))
tail(sort(colMeans(KmeansCluster[[2]])))
tail(sort(colMeans(KmeansCluster[[3]])))
tail(sort(colMeans(KmeansCluster[[4]])))
tail(sort(colMeans(KmeansCluster[[5]])))
tail(sort(colMeans(KmeansCluster[[6]])))
tail(sort(colMeans(KmeansCluster[[7]])))

#Find similarities between hierarchical and k-means cluster groups

