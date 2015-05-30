#Load the dataset
wiki <- read.csv("wiki", stringsAsFactors = FALSE)
wiki$Vandal <- as.factor(wiki$Vandal)
wiki$Minor <- as.factor(wiki$Minor)
wiki$Loggedin <- as.factor(wiki$Loggedin)

#Exploratory Data Analysis
str(wiki)
table(wiki$Vandal) #Cases of vandalism in the history of this page

#Bag of Words Approach
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)

#Keep only the terms that appear in 0.3% or more of the revisions
sparseAdded = removeSparseTerms(dtmAdded, 0.997) #1-0.3/100

#Create the dataframe from the matrix
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

#perform bag of words approach for removed words
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#Combine the two dataframes and Vandal column
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

#Sppit the data
library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, spl==TRUE)
wikiTest = subset(wikiWords, spl==FALSE)

#Compute accuracy of the baseline method
table(wikiTest$Vandal) #618/(618+545) = 0.531

#Build a CART model to predict Vandal, using all of the other variables as independent variables. 

wikiCART = rpart(Vandal ~ ., data=wikiTrain, method="class")
testPredictCART = predict(wikiCART, newdata=wikiTest, type="class")
table(wikiTest$Vandal, testPredictCART) #Accuracy is (618+12)/(618+533+12) = 0.5417

#Plot the tree
prp(wikiCART) #you can see that the tree uses two words: "R arbitr" and "R thousa"


#Although it beats the baseline, bag of words is not very predictive of the model. we will try two techniques - identifying a key class of words, and counting words
#Create a copy of your dataframe from the previous question:
wikiWords2 = wikiWords

#Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

#Split the dataset and create the CART model
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
testPredictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, testPredictCART2)

#Another possibility is that the number of words added and removed is predictive, perhaps more so than the actual words themselves. 
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

#Create the CART model
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART3) #Accuracy is 0.6552021.

#use metadata to create the model
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, spl==TRUE)

wikiTest4 = subset(wikiWords3, spl==FALSE)
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
predictTestCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictTestCART4) #(595+241)/(595+23+304+241) = 0.7188306. By adding new variables we were able to significantly improve the quality of our model








