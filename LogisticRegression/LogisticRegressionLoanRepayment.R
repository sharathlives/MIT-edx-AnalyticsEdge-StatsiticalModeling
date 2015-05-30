#Load the dataset
loans <- read.csv("loans.csv")

#preparing the dataset
str(loans)
summary(loans)
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))

#Create the imputed dataset to fill in missing values
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

#Create model. Note that some have a positive coefficient (meaning that higher values of the variable lead to an increased risk of defaulting) and some have a negative coefficient (meaning that higher values of the variable lead to a decreased risk of defaulting).
set.seed(144)
library(caTools)
set.seed(144)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)
mod = glm(not.fully.paid~., data=train, family="binomial")
summary(mod)

#Interpreting the model
#Consider two loan applications, which are identical other than the fact that the borrower in Application A has FICO credit score 700 while the borrower in Application B has FICO credit score 710.
#Because Application A is identical to Application B other than having a FICO score 10 lower, its predicted log odds differ by -0.009317 * -10 = 0.09317 from the predicted log odds of Application B.

#What is the value of O(A)/O(B)?
#Using the answer from the previous question, the predicted odds of loan A not being paid back in full are exp(0.09317) = 1.0976 times larger than the predicted odds for loan B. 

#Compute the confusion matrix
predicted.risk <- predict(mod, newdata = test, type = "response")
table(test$not.fully.paid, preidcted.risk)
accuracy <- 2403/2873 #0.8364
accuracy_baseline <- 2413/2873 #0.8399

#Compute test set AUC
library(ROCR)
pred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

#Creating a smart baseline
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)
cor(train$int.rate, train$fico)

#Make test predictions for the bivariate model
pred.bivariate = predict(bivariate, newdata=test, type="response")
summary(pred.bivariate)

prediction.bivariate = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(prediction.bivariate, "auc")@y.values)

#Computing the proftability of an investment
#How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest?

10*exp(0.06*3) #11.97.

#Profit for the investor if it paid back in full. c * exp(rt) - c 
#Profit when nothing is paid back. -c

#In order to evaluate the quality of an investment strategy, we need to compute this profit for each loan in the test set.
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

#Max profit
max(test$profit)
mean(test$profit) #returns from all the loans if all loans received same some of money

#To meet this objective, we will analyze an investment strategy in which the investor only purchases loans with a high interest rate (a rate of at least 15%), but amongst these loans selects the ones with the lowest predicted risk of not being paid back in full. We will model an investor who invests $1 in each of the most promising 100 loans.
highInterest = subset(test, int.rate >= 0.15)
summary(highInterest$profit)
table(highInterest$not.fully.paid)

#Investment strategy based on risk
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100] #100th smallest predicted probability of not paying in full

#What is the profit of the investor, who invested $1 in each of these 100 loans 
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)







