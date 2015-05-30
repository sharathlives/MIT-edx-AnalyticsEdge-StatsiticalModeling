#we will look at whether data from previous auctions on eBay, a major online auction and shopping site, can be used to predict whether a new item will be sold at some target price.

#Load the data
eBay <- read.csv("ebay.csv", stringsAsFactors = FALSE)

#Proportion sold
table(eBay$sold)

#Convert to factor
eBay$sold <- as.factor(eBay$sold)
eBay$condition <- as.factor(eBay$condition)
eBay$heel <- as.factor(eBay$heel)
eBay$style <- as.factor(eBay$style)
eBay$color <- as.factor(eBay$color)
eBay$material <- as.factor(eBay$material)

#Which of the following methods requires the dependent variable be stored as a factor variable when training a model for classification?
#Random Forests

#Split the dataset into test and train
set.seed(144)
library(caTools)
spl = sample.split(eBay$sold, 0.7)
training <- subset(eBay, spl == TRUE)
testing <- subset(eBay, spl == FALSE)

#training a logistic regression model
model <- glm(sold ~ biddable+startprice+condition+heel+style+color+material, data = eBay, family = "binomial")
summary(model)

#Probaibility of being sold given a shoe that is not for auction (biddable=0), that has start price $100, that is in condition "Pre-owned", that has "High" heels, that has style "Open Toe", that has color "Black", and that has material "Satin".
df <- data.frame(0, 100, "Pre-owned", "High", "Open Toe", "Black", "Satin")
colnames(df) <- c("biddable", "startprice", "condition", "heel", "style", "color", "material")
predict(model, newdata = df)

#The coefficients of the model are the log odds associated with that variable; so we see that the odds of being sold are exp(0.8325406)=2.299153 those of an otherwise identical shoe in the baseline category for the style variable (which is "Open Toe"). This means the stiletto is predicted to have 129.9% higher odds of being sold.
testPred <- predict(model, newdata = testing, type = "response")
table(testing$sold, testPred >= 0.5)

#What is the test set AUC
library(ROCR)
pred = prediction(testPred, testing$sold)
as.numeric(performance(pred, "auc")@y.values)

#The proportion of the time the model can differentiate between a randomly selected shoe that was sold and a randomly selected shoe that was not sold

#Which logistic regression threshold is associated with the upper-right corner of the ROC plot (true positive rate 1 and false positive rate 1)?
#A model with threshold 0 predicts 1 for all observations, yielding a 100% true positive rate and a 100% false positive rate.

#At roughly which logistic regression cutoff does the model achieve a true positive rate of 80% and a false positive rate of 50%?
ROCRpred = prediction(testPred, testing$sold)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE)

#Cross-vaidation paramteres
#Which of the following best describes how 10-fold cross-validation works when selecting between 3 different parameter values?
#30 models are trained on subsets of the training set and evaluated on a portion of the training set 

#Set the random seed to 144 (even though you have already done so earlier in the problem). Then use the caret package and the train function to perform 10-fold cross validation with the data set train to select the best cp value for a CART model that predicts the dependent variable using "biddable", "startprice", "condition", "heel", "style", "color", and "material". Select the cp value from a grid consisting of the 50 values 0.001, 0.002, ..., 0.05.
#What cp value maximizes the cross-validation accuracy?
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.001,0.05,0.001))
train( sold ~ biddable+startprice+condition+heel+style+color+material , data = training, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

#Build and plot the CART model
model <- rpart(sold ~ biddable+startprice+condition+heel+style+color+material , data = training, cp = 0.005, method = "class")
prp(model)

#Include text analytics in the model
corpus <- Corpus(VectorSource(eBay$description))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
doc = DocumentTermMatrix(corpus)
doc

#How many unique word stems are in dtm? 10907

#Remove all terms that don't appear in at least 10% of documents in the corpus, storing the result in a new document term matrix called spdtm.
spdtm = removeSparseTerms(doc, 0.9) #1-10/100 is 0.9
spdtm #144

#Convert spdtm to a data frame called descriptionText. Which word stem appears the most frequently across all descriptions?
descriptionText = as.data.frame(as.matrix(spdtm))
which.max(colSums(descriptionText))

#Add a "D" in front of all the variable names in descriptionText:
names(descriptionText) = paste0("D", names(descriptionText))

#Copy the following variables from the eBay data frame into descriptionText:
#sold, biddable, startprice, condition, heel, style, color, material
descriptionText$sold <- eBay$sold
descriptionText$biddable <- eBay$biddable
descriptionText$startprice <- eBay$startprice
descriptionText$condition <- eBay$condition
descriptionText$heel <- eBay$heel
descriptionText$style <- eBay$style
descriptionText$color <- eBay$color
descriptionText$material <- eBay$material

#Split descriptionText into a training set called trainText and a testing set called testText using the variable "spl" that was earlier used to split eBay into train and test.
set.seed(144)
library(caTools)
spl = sample.split(descriptionText$sold, 0.7)
training <- subset(descriptionText, spl == TRUE)
testing <- subset(descriptionText, spl == FALSE)

#How many variables are in testText? #152
length(testing)

#Using trainText, train a logistic regression model called glmText to predict the dependent variable using all other variables in the data frame.
glmText <- glm(sold ~., data = training, family = binomial)


#How many of the word frequencies from the description text (variables beginning with the letter "D") are significant at or below the p=0.05 level? 
summary(glmText)

#What is the training-set AUC of the new logistic regression model?
trainPred <- predict(glmText, newdata = training, type = "response")
pred = prediction(trainPred, training$sold)
as.numeric(performance(pred, "auc")@y.values)

##What is the testing-set AUC of the new logistic regression model?
testPred <- predict(glmText, newdata = testing, type = "response")
pred = prediction(testPred, testing$sold)
as.numeric(performance(pred, "auc")@y.values)
