#Load the data. This assignment links to the logistic regression assignment on election forecasting

library(ggplot2)
library(ggmap)
library(maps)
states_map <- map_data("state") #map_data contains other built-in maps, including a US county map, a world map, and maps for France and Italy.

#Exploratory data analysis
table(states_map$group) #group, defines the different shapes or polygons on the map. The variable "order" defines the order to connect the points within each group

#Create the states map
ggplot(states_map, aes(x = lat, y = long, group = group)) + geom_polygon(fill = "white", color = "black") #fill defines the background and color indicates the borders

#Create the regression model predictions
Train <- subset(polling, Year == 2004 | Year == 2008)
Test <- subset(polling, Year == 2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

#For how many states is the binary prediction 1. What is the average prediction
table(TestPredictionBinary)
summary(TestPrediction)

#Merging the states_map and the prediction map datasets
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

#Map shows we incorrectly predicted Florida as this differed from the actual result
s <- subset(predictionMap, Test.State == "Florida")
mean(s$TestPrediction)


#Colorign the states map dataset
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#Explore parameter setting of geom_polygon, Vary size and linetype and alpha
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

