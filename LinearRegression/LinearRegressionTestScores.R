#Load the Test Scores datasets
pisaTest <- read.csv("pisa2009test.csv")
pisaTrain <- read.csv("pisa2009train.csv")

#We want to find the factors that result in test scores

#what is the average reading test score of males
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

#Which variables are missing data in at least one observation in the training set?
summary(pisaTrain)

#Remove missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

#To include unordered factors in a linear regression model, we define one level as the "reference level" 
#and add a binary variable for each of the remaining levels. 
#In this way, a factor with n levels is replaced by n-1 binary variables. 
#The reference level is typically selected to be the most frequently occurring level in the dataset.

#by default R selects the first level alphabetically ("American Indian/Alaska Native") as the reference level of our factor instead of the most common level ("White"). 
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")


#Build the model
lmScore <- lm(readingScore ~., data = pisaTrain)
summary(lmScore)

#Compute the root mean squared error
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain))

#Consider two students A and B. They have all variable values the same, except that student A is in grade 11 and student B is in grade 9. What is the predicted reading score of student A minus the predicted reading score of student B?
Beta coefficient of Grade * (Grade A - Grade B) = 29.542707*2 =  59.08541

#What is the meaning of the coefficient associated with variable raceethAsian? Predicted difference in the reading score between an Asian student and a white student who is otherwise identical 

#What are the variables that can be removed
#From summary(lmScore), we can see which variables were significant at the 0.05 level. Because several of the binary variables generated from the race factor variable are significant, we should not remove this variable.

#Using the "predict" function and supplying the "newdata" argument, use the lmScore model to predict the reading scores of students in pisaTest.
predTest = predict(lmScore, newdata=pisaTest)

#What is the range between the maximum and minimum predicted reading score on the test set?
summary(predTest)

#What is the root-mean squared error (RMSE) of lmScore on the testing set?
SSE = sum((predTest - pisaTest$readingScore)^2)
RMSE = sqrt(SSE / nrow(pisaTest))

#What is the R-squared value of the test set
SST = sum( (mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
R2 = 1 - SSE/SST

#What is the predicted test score used in the baseline model?
baseline = mean(pisaTrain$readingScore)

#What is the sum of squared errors of the baseline model on the testing set? 
SST = sum( (mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)

