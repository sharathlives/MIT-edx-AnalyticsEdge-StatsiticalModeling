#Load the dataset
data(state)
statedata = data.frame(state.x77)
head(statedata)

#Build a linear regression model
model <- lm(Life.Exp ~. , data = statedata)
summary(model) #Adjusted R-squared 0.6922 
sum(model$residuals^2) #sum of squared errors 23.29714
RegModel2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=statedata) #best model
summary(RegModel2) #Adjusted R-squared 0.7126
sum(model$residuals^2) #sum of squared errors 23.29714

#Therefore, Trying different combinations of variables in linear regression is like trying different numbers of splits in a tree - this controls the complexity of the model. 

#Build a CART model. This is a regression tree so no method = class
CARTmodel <- rpart(Life.Exp ~. , data = statedata)
PredictionsCART = predict(CARTmodel)
sum((predict(CARTmodel) - statedata$Life.Exp)^2)
CARTmodel <- rpart(Life.Exp ~. , data = statedata, minbucket = 5) 
sum((predict(CARTmodel) - statedata$Life.Exp)^2)#23.64
CARTmodel3 = rpart(Life.Exp ~ Area, data=statedata, minbucket=1)
sum((statedata$Life.Exp - predict(CARTmodel3))^2) #9.312442.

#Cross-validation to tune the model
library(caret)
set.seed(111)
fitControl = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01) )
train(Life.Exp ~ ., data=statedata, method="rpart", trControl = fitControl, tuneGrid = cartGrid)

#Fot the model with the tuned cp value
CARTmodel4 = rpart(Life.Exp ~ ., data=statedata, cp=0.12)
prp(CARTmodel4)
PredictionsCART4 = predict(CARTmodel4)
sum((statedata$Life.Exp - PredictionsCART4)^2) #SSE is 32.86549





