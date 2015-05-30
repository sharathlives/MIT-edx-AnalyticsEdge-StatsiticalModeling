#Load the dataset
songs <- read.csv("songs.csv")

#Understanding the data
str(songs)
songsMJ[songsMJ$Top10 == 1, ]$songtitle #MJ top 10
nrow(songs2010 <- songs[songs$year == 2010, ]) #2010 songs
songs[which.max(songs$tempo),]$songtitle #highest tempo song

#Subsetting the data into train and test datasets
SongsTrain <- subset(songs, year <= 2009)
SongsTesttest <- subset(songs, year = 2010)

#Create first model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

#Identify multicollineairty
cor(SongsTest$loudness, SongsTest$energy)

#Create second model
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

#Create thrid model
SongsLog2 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog2)

#Create prediction
predictTest = predict(SongsLog2, type="response", newdata=SongsTest)
table(SongsTest$Top10, predictTest > 0.45)

#Solvefor accuracy, sensitivity, specificity and error  rate
table(SongsTest$Top10, predictTest > 0.45)
sensitivity <- 15/(44+15)
specificity <- 311/314

#Model 3 provides conservative predictions, 
#and predicts that a song will make it to the Top 10 very rarely. 
#So while it detects less than half of the Top 10 songs, 
#we can be very confident in the songs that it does predict to be Top 10 hits.