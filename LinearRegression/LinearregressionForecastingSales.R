#In this problem, we will try to predict monthly sales of the Hyundai Elantra in the United States. 

#Prepare the dataset
Elantra = read.csv("elantra.csv")
ElantraTrain = subset(Elantra, Year <= 2012)
ElantraTest = subset(Elantra, Year > 2012)

#Build a linear regression model to predict monthly Elantra sales using Unemployment, CPI_all, CPI_energy and Queries as the independent variables. 
ElantraLM = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, data=ElantraTrain)
summary(ElnatraLM) #There are no variables that are significant

#Interpreting the coefficients
#For an increase of 1 in Unemployment, the prediction of Elantra sales decreases by approximately 3000.

#Modeling seasonality
#In modeling demand and sales, it is often useful to model seasonality. Seasonality refers to the fact that demand is often cyclical/periodic in time.
#In our problem, since our data includes the month of the year in which the units were sold, it is feasible for us to incorporate monthly seasonality. From a modeling point of view, it may be reasonable that the month plays an effect in how many Elantra units are sold.

ElantraLM = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data=ElantraTrain)
#The second option is correct: the adjusted R-Squared is the R-Squared but adjusted to take into account the number of variables. If the adjusted R-Squared is lower, then this indicates that our model is not better and in fact may be worse. Furthermore, if none of the variables have become significant, then this also indicates that the model is not better.

#Understanding the new model
#In the new model, given two monthly periods that are otherwise identical in Unemployment, CPI_all, CPI_energy and Queries, what is the absolute difference in predicted Elantra sales given that one period is in January and one is in March?
#221.38

#In the new model, given two monthly periods that are otherwise identical in Unemployment, CPI_all, CPI_energy and Queries, what is the absolute difference in predicted Elantra sales given that one period is in January and one is in May?
#442.76

#Numeric Vs. Factors
#we must convert Month to a factor variable before adding it to the model.By modeling Month as a factor variable, the effect of each calendar month is not restricted to be linear in the numerical coding of the month.
#The previous subproblem essentially showed that for every month that we move into the future (e.g, from January to February, from February to March, etc.), our predicted sales go up by 110.69. This isn't right, because the effect of the month should not be affected by the numerical coding, and by modeling Month as a numeric variable, we cannot capture more complex effects.

#Build the new model with month as a factor variable
ElantraTrain$MonthFactor = as.factor(ElantraTrain$Month)
ElantraTest$MonthFactor = as.factor(ElantraTest$Month)
ElantraLM = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFactor, data=ElantraTrain)
summary(ElantraLM)
#We see that most variables now are significant

#Multi-collinearity
#changes in coefficient signs and signs that are counter to our intuition may be due to a multicolinearity problem. 
cor(ElantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])
#The high correlations between CPI_energy and the other variables are -0.80071881 (Unemployment), 0.8328381 (Queries) and 0.91322591 (CPI_all).


#Build a reduced model
#Remove the variable with the highest p-value (i.e., the least statistically significant variable) from the model. 
#The variable with the highest p-value is "Queries". After removing it and looking at the model summary again, we can see that there are no variables that are insignificant, at the 0.10 p-level. 

#Using the model from Problem 6.1, make predictions on the test set. What is the sum of squared errors of the model on the test set?
PredictTest = predict(ElantraLM, newdata=ElantraTest)
SSE = sum((PredictTest - ElantraTest$ElantraSales)^2)

#he baseline method that is used in the R-Squared calculation (to compute SST, the total sum of squares) simply predicts the mean of ElantraSales in the training set for every observation (i.e., without regard to any of the independent variables).
#14462.25

#Find test set R-squared
#0.7280232

#What is the largest absolute error that we make in our test set predictions?
max(abs(PredictTest - ElantraTest$ElantraSales)) #7491.488

#In which period (Month,Year pair) do we make the largest absolute error in our prediction?
which.max(abs(PredictTest - ElantraTest$ElantraSales))







