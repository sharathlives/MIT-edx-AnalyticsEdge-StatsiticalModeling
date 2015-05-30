#Load the census dataset
census <- read.csv("census.csv")

#Build the log regression model
library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)
censusglm = glm( over50k ~ . , family="binomial", data = train)


#Evaluate the log model
summary(censusglm)
predictTest = predict(censusglm, newdata = test, type = "response")
table(test$over50k, predictTest >= 0.5) #Accuracy is (9051+1888)/(9051+662+1190+1888) = 0.8552107
table(test$over50k) #baseline accuracy of the model is 9713/(9713+3078) = 0.7593621. Therefore, the model achieves a high accuracy

library(ROCR)
ROCRpred = prediction(predictTest, test$over50k) 
as.numeric(performance(ROCpred, "auc")@y.values) #AUC is 0.9061598

#Build the CART model
library(rpart)
library(rpart.plot)
censustree = rpart( over50k ~ . , method="class", data = train)
prp(censustree) #There are 4 splits in the tree

#Evaluate the CART model
predictTest = predict(censustree, newdata = test, type = "class")
table(test$over50k, predictTest) #Accuracy is (9243+1596)/(9243+470+1482+1596) = 0.8473927. CART does slightly worse than log regression, however is much more interpretable

#Compute the accuracy of the CART model
predictTest = predict(censustree, newdata = test)
predictTest = predictTest[,2]
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

#Create a random forest model
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
censusrf = randomForest(over50k ~ . , data = trainSmall)
predictTest = predict(censusrf, newdata=test)
table(test$over50k, predictTest) # this is the accuracy of the model (9614+1050)/nrow(test) = 0.8337112

#Evalauate the random forest model

#Find the number of times variables were used to split in the random forest model
vu = varUsed(censusrf, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))

#Evaluate impurity in the random forest model. A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. 
varImpPlot(censusrf) #occupation is the most important to reduce impurity

#Find the right cp value using k-fold cross-validation
library(caret)
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 ) #Specify that we are going to use k-fold cross validation with 10 folds
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) #Specify the grid of cp values that we wish to evaluate:
train( over50k ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid ) #cp = 0.002 is the best value
model <- train(classe ~., data = train, method = "rf", prox = TRUE, 
               trControl = trainControl(method = "cv", number = 4, allowParallel = TRUE))


#Fit CART model with new cp values
test_pred <- predict(model, test)
confusionMatrix(test_pred, test$classe)

#Predictions for the assignment
pred_testing <- predict(model, testing)
pred_testing <- as.character(pred_testing)
pred_testing








