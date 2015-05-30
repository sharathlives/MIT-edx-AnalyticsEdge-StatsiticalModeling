#Loading the data

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
str(NewsTrain)
str(NewsTest)

#This is an example and wasnot used in the final model
#Simple logistic regression model. Basic model on WordCount
#SimpleMod = glm(Popular ~ WordCount, data=NewsTrain, family=binomial)

#Test the accuracy of this simple model on the trainign set because we dont have response model in the testing set
#PredictTrainSimple = predict(SimpleMod, newdata = NewsTrain, type = "response")
#table(NewsTrain$Popular, PredictTrainSimple > 0.5)

#Make predictions on simple model
#PredTest = predict(SimpleMod, newdata=NewsTest, type="response")

#Create the files to be uploaded
#MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
#write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)

#Now improve on results from the simple model
## To convert the date/time we use strptime
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

##Breaks time variable into weekday, month, year
NewsTrain$Weekday = as.factor(NewsTrain$PubDate$wday+1) #To account for weekly seasonality
NewsTrain$Month = as.factor(NewsTrain$PubDate$mon+1) #To account for monthly seasonality
NewsTrain$Year = as.factor(NewsTrain$PubDate$year+1900) #To account for any yearly patterns

NewsTest$Weekday = as.factor(NewsTest$PubDate$wday+1)
NewsTest$Month = as.factor(NewsTest$PubDate$mon+1)
NewsTest$Year = as.factor(NewsTest$PubDate$year+1900)

#Make char variables into factor variables
NewsTrain$NewsDesk <- as.factor(NewsTrain$NewsDesk)
NewsTest$NewsDesk <- as.factor(NewsTest$NewsDesk)
NewsTrain$SectionName <- as.factor(NewsTrain$SectionName)
NewsTest$SectionName <- as.factor(NewsTest$SectionName)
NewsTrain$SubsectionName <- as.factor(NewsTrain$SubsectionName)
NewsTest$SubsectionName <- as.factor(NewsTest$SubsectionName)

#Text Analytics
#Add words from the headline that were most popular

#Create the corpus for headline
library(tm)
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

CorpusHeadline = tm_map(CorpusHeadline, tolower) #Convert to lower case
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument) #Additional line
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation) #Remove punctuation
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english")) #Remove stopr words
CorpusHeadline = tm_map(CorpusHeadline, stemDocument) #example, if the word is 'worked' then remove work

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparse))

# Check the colnames 
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))
HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

# Before building models, we want to add back the original variables from our datasets. We'll add back the dependent variable to the training set, and the WordCount variable to both datasets. You might want to add back more variables to use in your model - we'll leave this up to you!
HeadlineWordsTrain$Popular = NewsTrain$Popular
HeadlineWordsTrain$Popular = as.factor(HeadlineWordsTrain$Popular)
HeadlineWordsTrain$WordCount = NewsTrain$WordCount
HeadlineWordsTest$WordCount = NewsTest$WordCount

HeadlineWordsTrain$Weekday= NewsTrain$Weekday  
#HeadlineWordsTrain$Month = NewsTrain$Month 
#HeadlineWordsTrain$Year = NewsTrain$Year 

HeadlineWordsTest$Weekday= NewsTest$Weekday  
#HeadlineWordsTest$Month = NewsTest$Month 
#HeadlineWordsTest$Year = NewsTest$Year 

HeadlineWordsTrain$NewsDesk <- NewsTrain$NewsDesk
#HeadlineWordsTrain$SectionName <- NewsTrain$SectionName
#HeadlineWordsTrain$SubsectionName <- NewsTrain$SubsectionName

HeadlineWordsTest$NewsDesk <- factor(NewsTest$NewsDesk, levels = levels(HeadlineWordsTrain$NewsDesk))
#HeadlineWordsTest$SectionName <- factor(NewsTest$SectionName, levels = levels(HeadlineWordsTrain$SectionName))
#HeadlineWordsTrain$SubsectionName <- NewsTrain$SubsectionName

# Now let's create a random forest model using all of the variables:
HeadlineWordsLog = randomForest(Popular ~ ., data=HeadlineWordsTrain,ntree=200, nodesize=25)


#Check the accuracy of our model on the training set because we cannot check it on the test set
PredictTrain = predict(HeadlineWordsLog, newdata = HeadlineWordsTrain)
table(HeadlineWordsTrain$Popular, PredictTrain)

# And make predictions on our test set:
PredTest = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")

# Now we can prepare our submission file for Kaggle:
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)
