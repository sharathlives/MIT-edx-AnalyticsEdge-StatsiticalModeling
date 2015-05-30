#Load the dataset
climate_change <- read.csv("climate_change.csv")

#We are interested in how changes in these variables affect future temperatures, as well as how well these variables explain temperature changes so far.

#Subset the dataset into test and training datasets
training <- subset(climate_change, Year <= 2006)
test <- subset(climate_change, Year > 2006)

#Construct the linear model
model <- lm(Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data = training)
summary(model)

#Which variables are significant in the model
#We consider a variable to be significant only if the p-value is below 0.05. 
#If you look at the model we created in the previous problem using summary(climatelm), all of the variables have at least one star except for CH4 and N2O. So MEI, CO2, CFC.11, CFC.12, TSI, and Aerosols are all significant.

#All of the gas concentration variables reflect human development - N2O and CFC.11 are correlated with other variables in the data set. 
#What are CFC.11 and N20 highly correlated with?
cor(training)

#Given that the correlations are so high, let us focus on the N2O variable and build a model with only MEI, TSI, Aerosols and N2O as independent variables.
reduced_model <- lm(Temp ~ MEI+TSI+Aerosols+N2O, data = training)
summary(reduced_model)

#Use the step function to automatically simplify the model. The consequence of this is that the step function will not necessarily produce a very interpretable model - just a model that has balanced quality and simplicity for a particular weighting of quality and simplicity (AIC)
step_model <- step(model)
summary(step_model)

#Using the model produced from the step function, calculate temperature predictions for the testing data set, using the predict function.
#Calcuate the R-squared

tempPredict = predict(step_model, newdata = test)
SSE = sum((tempPredict - test$Temp)^2)
SST = sum( (mean(training$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST
