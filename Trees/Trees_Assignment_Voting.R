#Load the dataset
gerber <- read.csv("gerber.csv")

#Exploration
str(gerber)
table(gerber$voting); 1- mean(gerber$voting) #proportion of voters in the election; baseline accuracy
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$neighbors, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$control, mean)

#Build the model
LogModel = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")
summary(LogModel)

#Compute accuracy
predictLog = predict(LogModel, type="response")
table(gerber$voting, predictLog > 0.3) #accuracy = 0.542 
table(gerber$voting, predictLog > 0.5) #accuracy = 0.6
1- mean(gerber$voting) #Since accuracy of the model is less than the baseline accuracy, this is a weak predictive model

#Build a CART model
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber) #this is a regression tree. we haven't specified mathod = class

#Build another CART model based on cross validation. 
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
#this shows that 0.31 percent of the civic duty people voted (<0.5 - no)

#Build another tree that has the sex variable included
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
#this shows that males are more likely to vote than females

#Create a tree to understand how the interaction terms are handled
CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0)
CARTsex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTcontrol, digits=6)
control <- subset(gerber, control == 1); tapply(control$gender, control$voting, mean)
tapply <- tapply(control$voting, control$sex, mean) #gices the same result as the rgeression tree
#The split says that if control = 1, predict 0.296638, and if control = 0, predict 0.34. The absolute difference between these is 0.043362.
#Using the second tree (with control and sex), determine who is affected more by NOT being in the control group (being in any of the four treatment groups)
#We see that men and women are affected about the same if they are in the control group


#Create a logistic regression to understand how interaction terms are handled
LogModelSex = glm(voting ~ control + sex, data=gerber, family="binomial") #The coefficent for sex is negative indicating that women are less likely to vote. Women have a larger variable in the sex variable
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")
#The four values in the results correspond to the four possibilities in the order they are stated above ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) ). There is very little differnece between the tree and the logistic regression

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
#Negative value of the interaction term means, If a person is a woman and in the control group, the chance that she voted goes down.

predict(LogModel2, newdata=Possibilities, type="response") #The logistic regression model now predicts 0.2904558 for the (Woman, Control) case, so there is now a very small difference (practically zero) between CART and logistic regression.












