# FORECASTING AIRLINE DELAYS
# 
# On any given day, more than 87,000 flights take place in the United States alone. 
# About one-third of these flights are commercial flights, operated by companies like United, American Airlines, and JetBlue.
# While about 80% of commercial flights take-off and land as scheduled, the other 20% suffer from delays due to various reasons. 
# A certain number of delays are unavoidable, due to unexpected events, but some delays could hopefully be avoided if the factors causing delays were better understood and addressed.

#Read the dataset
Airlines <- read.csv("AirlineDelay.csv")

#Split into training and testing set. we have frequently used the sample.split function to randomly split our data. The sample.split function is typically used to split data with a categorical dependent variable, and we have a continuous dependent variable. 
set.seed(15071)
spl = sample(nrow(Airlines), 0.7*nrow(Airlines))
AirlinesTrain = Airlines[spl,]
AirlinesTest = Airlines[-spl,]
nrow(AirlinesTrain)
nrow(AirlinesTest)

#Build a linear regression model
AirlinesModel <- lm(TotalDelay ~., AirlinesTrain)
summary(AirlinesModel) #R-squared is 0.09475. Identify significant variables. 

#Find correlation between NumPrevFlights and PrevFlightGap and OriginAvgWind and OriginWindGust
cor(AirlinesTrain$NumPrevFlights,AirlinesTrain$PrevFlightGap)
cor(AirlinesTrain$OriginAvgWind,AirlinesTrain$OriginWindGust)

#In the linear regression model, given two flights that are otherwise identical, what is the absolute difference in predicted total delay given that one flight is on Thursday and the other is on Sunday?
#6.989857

#n the linear regression model, given two flights that are otherwise identical, what is the absolute difference in predicted total delay given that one flight is on Saturday and the other is on Sunday?
#0.911413

#Build the test set predictions
#Make predictions on the test set using your linear regression model. 
predictTest <- predict(AirlinesModel, newdata = AirlinesTest)

#What is the Sum of Squared Errors (SSE) on the test set?
SSE = sum((AirlinesTest$TotalDelay- predictTest)^2)

#What is the Total Sum of Squares (SST) on the test set? 
mean(AirlinesTrain$TotalDelay) #Baseline model
SST = sum((AirlinesTest$TotalDelay - mean(AirlinesTrain$TotalDelay))^2)

#What is the R-squared on the test set?
1-1-SSE/SST

#What have we learnt from building the model
Since our R-squared values are low, we can conclude that our independent variables only explain a small amount of the variation in the dependent variable.

#Let us make this into a multi-class problem. Our new dependent variable will take three different values: "No Delay", "Minor Delay", and "Major Delay". 

#Create the delay variable
Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))

#How many airlines had delays
table(Airlines$DelayClass)

#Now, remove the original dependent variable "TotalDelay" from your dataset with the command:
Airlines$TotalDelay = NULL

#Then randomly split Airlines into a training set, containing 70% of the observations, and a testing set, containing 30% of the observations. 
set.seed(15071)
spl = sample.split(Airlines$DelayClass, SplitRatio = 0.7)
train = subset(Airlines, spl==TRUE)
test = subset(Airlines, spl==FALSE)

#Create a CART model to predict DelayClass
airlinesDelayTree = rpart(DelayClass ~ . , method="class", data = train)
prp(airlinesDelayTree) #There is 1 split in the tree

#Make predictions on the testing set
predictTest <- predict(airlinesDelayTree, newdata = test)
table(test$DelayClass, predictTest)
(153+1301)/(141+338+153+776+105+1301) #Accuracy of the model is 0.51

#Accuracy of baseline model
table(Airlines$DelayClass)
4688/nrow(Airlines)

#Conclusion: Out of the independent variables in our dataset, the best predictor of future delays is historical delays.


