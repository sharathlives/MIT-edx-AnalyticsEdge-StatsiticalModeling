#Read the datafile
mvt <- read.csv("mvtWeek1.csv")

#How many rows of data (observations) are in this dataset?
nrow(mvt)

#How many variables are in this dataset?
length(colnames(mvt))

#what is the maximum value of the variable "ID"?
max(mvt$ID)

#What is the minimum value of the variable "Beat"?
min(mvt$Beat)

#How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
table(mvt$Arrest)
summary(mvt$Arrest)

#How many observations have a LocationDescription value of ALLEY?
table(mvt$LocationDescription)

#Understanding Dates in R
#Convert data to date object

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$DateConvert <- DateConvert

#What is the median date
summary(DateConvert)

#Extract month and date and add this to the mvt dataframe
mvt$month <- months(DateConvert)
mvt$weekday <- weekdays(DateConvert)

#Replace transformed date
mvt$Date = DateConvert

#In which month did the fewest motor vehicle thefts occur?
which.min(table(mvt$month))
which.max(table(mvt$weekday))

#Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvt$Arrest,mvt$Month)
t <- subset(mvt, Arrest == TRUE)
which.max(table(t$month))


#Does crime increase or decrease in the time period
hist(mvt$DateConvert, breaks=100)

#Does the rate of arrests change over time
boxplot(mvt$DateConvert ~ mvt$Arrest)

#For what proportion of motor vehicle thefts in 2001 and 2007 was an arrest made?
temp <- table(mvt$Year, mvt$Arrest)
temp[1,2]/(temp[1,2] + temp[1,1])

temp[7,2]/(temp[7,2] + temp[7,1])

#Find the top five locations where motor vehicle thefts occur
sort(table(mvt$LocationDescription))

#How many observations are in Top5?
Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")
str(Top5)
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription %in% TopLocations)

Top5$LocationDescription = factor(Top5$LocationDescription)

#One of the locations has a much higher arrest rate than the other locations. Which is it?
table(Top5$LocationDescription, Top5$Arrest)

#On which day of the week do the most motor vehicle thefts at gas stations happen?
table(Top5$LocationDescription, Top5$Weekday)

#On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
table(Top5$LocationDescription, Top5$Weekday)



