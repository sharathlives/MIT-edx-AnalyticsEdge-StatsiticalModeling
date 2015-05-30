#Load the dataset
airlines <- read.csv("airlinesCluster.csv")

#Exploratory Analysis
#which TWO variables have (on average) the smallest and largest values?
summary(airlines)

#Normalizing the data to ensure that the larger values donot dominate the clusering algorithm
preproc = preProcess(airlines) #preprocesses the data
airlinesNorm = predict(preproc, airlines) #does the normalization

#Build the hierarchical cluster model
distances <- dist(airlinesNorm, method = "euclidean")
clusterAirlines = hclust(distances, method = "ward.D")
plot(clusterAirlines)
hierGroups = cutree(clusterAirlines, k = 5)

#Compute averages for each of the clusters
tapply(airlines$Balance, hierGroups, mean)
tapply(airlines$QualMiles, hierGroups, mean)
tapply(airlines$BonusMiles, hierGroups, mean)
tapply(airlines$BonusTrans, hierGroups, mean)
tapply(airlines$FlightTrans, hierGroups, mean)
tapply(airlines$DaysSinceEnroll, hierGroups, mean)

#How would you define members in Cluster 1
#They have highest dayssinceenroll and have exhausted their flight miles
#Infrequent but loyal customers.

#How would you define members in Cluster 2
#They have high QualMiles, FlightMiles and FlightTrans
#Customers who have accumulated a large amount of miles, and the ones with the largest number of flight transactions

#how would you define members in Cluster 3
#Have high Balance, Bonus Miles, Bonus trans
#Customers who have accumulated a large amount of miles, mostly through non-flight transactions.
  
#how would you define members in Cluster 4
#Relatively new customers who seem to be accumulating miles, mostly through non-flight transactions. 

#How would you define the members in Cluster 5
#Relatively new customers who don't use the airline very often.

#Run the K-means clustering on the normalized data
set.seed(88)
kmeansClust = kmeans(airlinesNorm, centers=5, iter.max=1000)
table(kmeansClust$cluster)

#Look at the average values
tapply(airlines$Balance, kmeansClust$cluster, mean)
tapply(airlines$QualMiles, kmeansClust$cluster, mean)
tapply(airlines$BonusMiles, kmeansClust$cluster, mean)
tapply(airlines$BonusTrans, kmeansClust$cluster, mean)
tapply(airlines$FlightTrans, kmeansClust$cluster, mean)
tapply(airlines$DaysSinceEnroll, kmeansClust$cluster, mean)
