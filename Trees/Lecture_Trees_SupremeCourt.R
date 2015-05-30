# Unit 4 - "Judge, Jury, and Classifier" Lecture


# VIDEO 4

# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model. method = class creates a classification tree instead of a regression tree
#min bucket indicates the largest number of observations in a tree split. Therefore higher the 
#minbucket parameter, more the data points in a tree split and lesses the number of tree splits
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)

prp(StevensTree)

# Make predictions. type = class for majority class predictions instead of probabilies; like using >= threshold value
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART) #Since type = "class",  here you dont need PredictCART>= 0.5 like in logistic regression
(41+71)/(41+36+22+71)

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
PredictROC # For each observation there is an outcome of 0 and an outcome of 1. Each observations
#is classified into a subset or bucket in our CART tree. These numbers gives the percentage of 
#training set data in that subset with outcome 0 and the percentage of trainign set data with outcome 1


pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)
#If you use table(Test$Reverse, PredictCART[,2] >= 0.5), you get the same confusion matrix as before

#Compute the test set AUC of this model
as.numeric(performance(pred, "auc")@y.values)


# VIDEO 5 - Random Forests

# Install randomForest package
install.packages("randomForest")
library(randomForest)

# Build random forest model. nodesize refers to the minbucket parameter. ntree = Number of trees  to build
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(40+74)/(40+37+19+74)



# VIDEO 6

# Install cross-validation packages. Used to estimate minbucket parameter. K-fold cross-validation
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 ) #cv stands for cross validation, number = 10 for 10 folds
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) #possbile values of our cp parameter

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)

