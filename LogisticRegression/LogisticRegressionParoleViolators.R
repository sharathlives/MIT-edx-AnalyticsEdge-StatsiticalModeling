#Load the dataset
parole <- read.csv("parole.csv")
str(parole)

#Preparing the dataset
table(parole$violator)
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

#Splitting into the testing and the training dataset
set.seed(144)
library(caTools)
split <- sample.split(parole$violator, SplitRatio = 0.7)
train <- subset(parole, split == TRUE)
test <- subset(parole, split == FALSE)

#Building a logistic regression model
paroleLog <- glm(violator ~ ., data = train, family = "binomial")


#Interpreting the model. Our model predicts that a parolee who has commited multiple offences 
#has a 5.01 exp(coefficient = 1.61) times higher odds of being a violator than a parolee who did not commit multiple offences
# but is otherwise identical. 
summary(paroleLog)


#Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months, had a maximum sentence of 12 months, 
#did not commit multiple offenses, and committed a larceny.

log(odds) = -4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*crime3 - 0.0117627*crime4
#male=1, race=1, age=50, state2=0, state3=0, state4=0, time.served=3, max.sentence=12, multiple.offenses=0, crime2=1, crime3=0, crime4=0
#log(odds) = -1.700629
#odds ratio = exp(-1.700629) = 0.183
#ProbabilityOfViloation <- 1/(1+exp(1.700629)) = 0.154

#Fit the model on the testing set
predictTest <- predict(paroleLog, newdata = test, type = "response")

#Evaluate the model with confusion matrix
table(test$violator, predictTest >= 0.5)
accuracy <- (167+12)/nrow(test)
sensitivity <- 12/23
specificity <- 167/179

#The job of a parole board is to make sure that a prisoner is ready to be released into free society, and therefore parole boards tend to be particularily 
#concerned with releasing prisoners who will violate their parole.
#The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5.

#Find the AUC or the area under the curve. The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator.
#The AUC deals with differentiating between a randomly selected positive and negative example.
library(ROCR)
pred = prediction(predictTest, test$violator)
as.numeric(performance(pred, "auc")@y.values)







