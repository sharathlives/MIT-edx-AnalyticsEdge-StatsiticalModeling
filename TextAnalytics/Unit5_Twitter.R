# Unit 5 - Twitter

#Bag of Words technique transforms text into independent variables. Counts the number of times each word  occurs in the text and uses this as individual variables. 

#Preprocessing the text
#Clean up irregularities (lower case, upper case, spelling mistakes)
#Punctuation (remove everything which is not a standard letter) 
#Stop words - Remove unhelpful terms. ex. who, that, which
#Stemming - Represent words with different endings as the same word. ex. arguing, argued, etc. - argue


# VIDEO 5

# Read in the data

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE) #stringsAsFactors ensures that the text is read properly. 
str(tweets)



# Create dependent variable. We want to find the tweets that were classified as negative by respondents.  

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)


# Install new packages tm, Snowballc to pre-process the data for the Bag of Words approach

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# Create corpus. Corpus is a collection of documents in the bag of words approach. Documents correspond to tweets in this example.
 
corpus = Corpus(VectorSource(tweets$Tweet))

# Look at corpus
corpus

corpus[[1]]


# Convert to lower-case

corpus = tm_map(corpus, tolower)

corpus[[1]]

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpus = tm_map(corpus, removePunctuation)

corpus[[1]]

# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]

# Stem document 

corpus = tm_map(corpus, stemDocument)

corpus[[1]]




# Video 6

# Create matrix

frequencies = DocumentTermMatrix(corpus)

frequencies

# Look at matrix 

inspect(frequencies[1000:1005,505:515])

# Check for sparsity. This will give you terms that appear atleast 20 times. 

findFreqTerms(frequencies, lowfreq=20)

# Remove sparse terms. These are terms that do not occur very frequently and will be removed. 

sparse = removeSparseTerms(frequencies, 0.995) #Only keep terms that appear in 0.5% or more of the tweets. Therfore, these words appear in 6 or more tweets
sparse

# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly. None of them should start with a number. 

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable

tweetsSparse$Negative = tweets$Negative

# Split the data

library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)



# Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")

table(testSparse$Negative, predictCART)

# Compute accuracy

(294+18)/(294+6+37+18)

# Baseline accuracy 

table(testSparse$Negative)

300/(300+55)


# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)

table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21)

