#read the dataset 
hubway <- read.csv("HubWayTrips.csv")

#Exploratory Analytics
nrow(hubway)
tapply(hubway$Duration, hubway$Weekday, mean)

#How many trips were taken in the morning, afternoon,evening
table(hubway$Morning) #60399
table(hubway$Afternoon) #74021 
table(hubway$Evening) #46264 
table(hubway$Male)[2]/nrow(hubway) #0.7371078 

#When clustering data, it is often important to normalize the variables so that they are all on the same scale. If you clustered this dataset without normalizing, which variable would you expect to dominate in the distance calculations?
#Otherwise, distance will dominate in the distance calculations

#Normalize the data
library(caret)
preproc = preProcess(hubway)
hubwayNorm = predict(preproc, hubway)

#Maximum value of duration and age
summary(hubwayNorm)

#We won't be using hierarchical clustering on this dataset because We might have too many observations in our dataset for Hierarchical clustering to handle

#Run the k-means clustering algorithm on your normalized dataset, selecting 10 clusters.
kmeansClust = kmeans(hubwayNorm, centers=10)
table(kmeansClust$cluster)
which.min(table(kmeansClust$cluster))
which.max(table(kmeansClust$cluster))

#Look at the average values to understand patterns
tapply(hubwayNorm$Duration, kmeansClust$cluster, mean)
tapply(hubwayNorm$Morning, kmeansClust$cluster, mean)
tapply(hubwayNorm$Afternoon, kmeansClust$cluster, mean)
tapply(hubwayNorm$Evening, kmeansClust$cluster, mean)
tapply(hubwayNorm$Weekday, kmeansClust$cluster, mean)
tapply(hubwayNorm$Male, kmeansClust$cluster, mean)
tapply(hubwayNorm$Age, kmeansClust$cluster, mean)

#Trips taken by female users in weekday evenings
#Cluster 10

#Which cluster best fits the description "leisurely (longer than average) afternoon trips taken on the weekends"?
#Cluster 8

#Which cluster best fits the description "morning trips taken by older male users"?
#Cluster 4

#Run the k-means clustering algorithm again, this time selecting 20 clusters. Right before the "kmeans" function, set the random seed to 8000.
kmeansClust = kmeans(hubwayNorm, centers=20)
table(kmeansClust$cluster)
which.min(table(kmeansClust$cluster))
which.max(table(kmeansClust$cluster))

#shorter than average trips that occur on weekday evenings
tapply(hubwayNorm$Duration, kmeansClust$cluster, mean)
tapply(hubwayNorm$Evening, kmeansClust$cluster, mean)
tapply(hubwayNorm$Weekday, kmeansClust$cluster, mean)






