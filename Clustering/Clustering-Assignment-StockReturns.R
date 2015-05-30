#Load the dataset
stocks <- read.csv("StocksCluster.csv")

#Exploratory Analysis
nrow(stocks)
table(stocks$PositiveDec)[2]/nrow(stocks) #0.54 - proportion of rows with positive returns in December
cor(stocks) #maximum corelation is between returns in Oct and returns in Nov
summary(stocks) #Which month (from January through November) has the largest mean return 

#Build a logistic regression model
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
stocksTrainModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)

#Find accuracy of the test and the training set
predictTrain = predict(stocksTrainModel, type="response", newdata=stocksTrain)
table(stocksTrain$PositiveDec, predictTrain > 0.5) #the overall  accuracy of the model on the training set is 0.57
predictTest = predict(stocksTrainModel, type="response", newdata=stocksTest)
table(stocksTest$PositiveDec, predictTest > 0.5)
PredictTest = predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, PredictTest > 0.5)
table(stocksTest$PositiveDec) #Baseline model gives an accuracy of 0.5460564

#Create a cluster with the data
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL #Remove dependent variable from the training set
limitedTest = stocksTest
limitedTest$PositiveDec = NULL #Remove dependent variable from the testing set

#Preprocess and normalize the data
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTrain) #check values after normalization
summary(normTest)

#Create k-means cluster
set.seed(144)
kmeansClust = kmeans(normTrain, centers=3, iter.max=1000)
table(kmeansClust$cluster)

#use the flexclust package to obtain training set and testing set cluster assignments for our observations
library(flexclust)
km.kcca = as.kcca(kmeansClust, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest) #test set observations assigned to each cluster

#subset the data
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
mean(stocksTrain1$PositiveDec); mean(stocksTrain2$PositiveDec); mean(stocksTrain3$PositiveDec)

#Build three seperate logistic regression models
stocksTrainModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
stocksTrainModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
stocksTrainModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)

summary(StocksModel1); summary(StocksModel2); summary(StocksModel3)

#Make predictions on the test set
PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type="response")

PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type="response")

PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type="response")

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)

table(stocksTest2$PositiveDec, PredictTest2 > 0.5)

table(stocksTest3$PositiveDec, PredictTest3 > 0.5)

#Cluster Specific Predictions
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5) #overall accuracy is 0.5788716


