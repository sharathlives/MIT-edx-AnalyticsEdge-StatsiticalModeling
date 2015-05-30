library(tm)

#Load and prepare the data
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))

#Exploratory analysis
length(unique(alltweets)) #unique words across all tweets

#As we can read from ?wordcloud, we will need to provide the function with a vector of words and a vector of word frequencies.This is obtained by colnames
#Each tweet represents a row in allTweets, and each word represents a column. Therefore, we need to access the sums of each column in allTweets, which is returned by colSums(allTweets).

#Build a word cloud
library(wordcloud)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25)) #Use scale to accomodate all the words. Look for warnings.

#Reprocess the data by removing apple and all the english stop words
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))

#Create a wordcloud of negaive tweets
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))

#min.freq and max.words are parameters that can be used to remove the least frequent words, resulting is a less cluttered word cloud.

#If random.order is set to FALSE, then the most frequent (largest) words will be plotted first, resulting in them being displayed together in the center of the word cloud.

#rot.per controls the proportion of words that are rotated to be vertical in the word cloud. By default 10% of words are rotated. 

#When random.color is set to TRUE, the words will be colored randomly. Otherwise, colors are based on number of appearances. 

#Add colors to the wordcloud
#YlOrRd is a "sequential palette," with earlier colors begin lighter and later colors being darker. Therefore, it is a good palette choice for indicating low-frequency vs. high-frequency words.



