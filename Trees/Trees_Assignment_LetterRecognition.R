#Load the dataset
letters <- read.csv("letters_ABPR.csv")

#We want to predict whether the letter is B or not. First, let's create an identifier that indicates if the letter is B or not
letters$isB <- as.factor(letters$letter == "B")
table(letters$isB)
2350/nrow(letters)

#Split the data into test and training
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

table(test$isB) #baseline accuracy is 0.754172

#Build the CART model to predict whether the letter is B or not
CARTb = rpart(isB ~ . - letter, data=train, method="class")
predictions = predict(CARTb, newdata=test, type="class")
table(test$isB, predictions) #accuracy is (1118+340)/nrow(test) = 0.9358151

#Build a random forest model to predict whether the letter is B or not
RFb = randomForest(isB ~ . - letter, data=train)
predictions = predict(RFb, newdata=test)
table(test$isB, predictions) #(1165+374)/nrow(test) = 0.9878049. We note that the random forest method improves significantly on the CART method

#Build a CART model to predict whether it is a letter or not
letters$letter <- as.factor(letters$letter)
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, spl == TRUE)
test2 = subset(letters, spl == FALSE)
table(train2$letter); 401/nrow(test) #therefore baseline accuracy of the model is 0.2573813.
CARTletter = rpart(letter ~ . - isB, data=train2, method="class")
predictLetter = predict(CARTletter, newdata=test2, type="class")
table(test2$letter, predictLetter) #Accuracy is (348+318+363+340)/nrow(test2) = 0.8786906. It is lower than the model to predict isB or not

#Build a random forest model to predict whether it is a letter or not
set.seed(1000)
RFletter = randomForest(letter ~ . - isB, data=train2)
predictLetter = predict(RFletter, newdata=test2)
table(test2$letter, predictLetter)
(390+380 +393+364)/nrow(test2) = 0.9801027 #Accuracy of random forests remained fairly high eventhough this is a multi-class problem now






