# Load the Anonymity Poll dataset
poll <- read.csv("AnonymityPoll.csv")

#ow many interviewees responded that they use a smartphone?
table(poll$Smartphone)
summary(poll$Smartphone)

#Which of the following are states in the Midwest census region?
table(poll$State, poll$Region)

#How many interviewees reported not having used the Internet and not having used a smartphone?
table(poll$Internet, poll$Smartphone)

#How many interviewees have a missing value for their Internet use?
table(is.na(poll$Internet))

#Use the subset function to obtain a data frame called "limited", which is limited to interviewees who reported Internet use or who reported smartphone use
limited <- subset(poll, poll$Internet == 1 | poll$Smartphone == 1)

#What is the average number of pieces of personal information on the Internet
mean(limited$Info.On.Internet)

#How many interviewees reported the maximum value of 11 for Info.On.Internet?
table(limited$Info.On.Internet)

#What proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the Internet? 
table(limited$Worry.About.Info)

#What proportion of interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the Internet?
table(limited$Anonymity.Possible)

#Build a histogram of the age of interviewees. What is the best represented age group in the population?
hist(limited$Age)

#What is the largest number of interviewees that have exactly the same value in their Age variable AND the same value in their Info.On.Internet variable?
plot(limited$Age, limited$Info.On.Internet)
table(limited$Age, limited$Info.On.Internet)
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

#What is the average Info.On.Internet value for smartphone users?
tapply(limited$Info.On.Internet, limited$Smartphone, mean)

#use tapply to break down the Tried.Masking.Identity variable for smartphone and non-smartphone users.
tapply(limited$Tried.Masking.Identity, limited$Smartphone, table)


