#Load the Flu Train dataset
FluTrain <- read.csv("FluTrain.csv")

#which week corresponds to the highest percentage of ILI-related physician visits? 
FluTrain[which.max(FluTrain$ILI), 1]

#Which week corresponds to the highest percentage of ILI-related query fraction?
FluTrain[which.max(FluTrain$Queries), 1]

#Plot the histogram of the dependent variable, ILI. 
hist(FluTrain$ILI)
#We conclude that most of the values are small with only a few large numbers (skew right)

#We will predict the natural log of the ILI variable, which can be computed in R using the log() 
#We use log transformation if there is a left or a right skew in the data
plot(log(FluTrain$ILI), FluTrain$Queries)

#Fit the model
train_model <- lm(log(ILI) ~ Queries, data = FluTrain )
summary(train_model)

#Load the Flu Test data
FluTest <- read.csv("FluTest.csv")

#Find predictions for the log ILI values
PredTest1 = predict(train_model, newdata=FluTest)

#Find predictions for the ILI values
PredTest1 = exp(predict(train_model, newdata = FluTest))

#Find the estimate for the percentage of ILI-related physician visits for the week of March 11, 2012? 
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]

#What is the relative error between predicted and observed values
ObservedValue <- FluTest$ILI[FluTest$Week == "2012-03-11 - 2012-03-17"] #Observed value
PredictedValue <- PredTest1[11]
(ObservedValue - PredictedValue)/ObservedValue

#What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of ILI-related physician visits, on the test set?
SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE <- sqrt(SSE/nrow(FluTest))

#Training a time series model. The zoo package has good functions to do time series analysis
install.packages("zoo")
library(zoo)

#Create a lag variable two weeks before because there is a two week lag in reporting ILI values. Therefore, wewould have justdata from two weeks before to report the present ILI value
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

#Use the plot() function to plot the log of ILILag2 against the log of ILI. 
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

#Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable as well as the log of the ILILag2 variable.
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

#Add ILILag2 variable to the FluTest dataframe
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

#Fill in the missing values for ILILag2 in FluTest. the ILILag2 variable for the first observation in FluTest should be he ILI value of the second-to-last observation in the FluTrain data frame
FluTest$ILILag2[1] = FluTrain$ILI[416]

#the ILILag2 variable for the second observation in FluTest should be The ILI value of the last observation in the FluTrain data frame
FluTest$ILILag2[2] = FluTrain$ILI[417]

#Obtain test set predictions of the ILI variable from the FluTrend2 model
predictedValueNew <- exp(predict(FluTrend2, newdata = FluTest))

#What is the RMSE of the predicted versus test variables
SSE = sum((predictedValueNew - FluTest$ILI)^2)
RMSE <- sqrt(SSE/nrow(FluTest))

#ARIMA models are a more general form of the model we built, which can include multiple lag terms as well as more complicated combinations of previous values of the dependent variable


