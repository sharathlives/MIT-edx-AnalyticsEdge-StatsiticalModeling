#Load the data

cps <- read.csv("CPSData.csv")

#what is the most common industry of employment?
which.max(table(cps$Industry))

#Which state has the fewest interviewees?
sort(table(cps$State))
which.min(table(cps$State))
which.max(table(cps$State))

#What proportion of interviewees are citizens of the United States?
1 - table(cps$Citizenship)[3]/nrow(cps)

#For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity? 
table(cps$Race, cps$Hispanic)

#Which variables have at least one interviewee with a missing (NA) value?
str(cps)

#Find a link between the missing values of the married variable with the Region or Age variable
table(cps$Region, is.na(cps$Married))
table(cps$Region, is.na(cps$Age))

#How many states had all interviewees living in a non-metropolitan area
table(cps$State, is.na(cps$MetroAreaCode))

#Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(cps$Region, is.na(cps$MetroAreaCode))/nrow(cps)

#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
sort(tapply(is.na(cps$MetroAreaCode), cps$State, mean))

#Load metro area codes and country codes
metro_area_map <- read.csv("MetroAreaCodes.csv")
country_map <- read.csv("CountryCodes.csv")

#How many observations (codes for metropolitan areas) are there in MetroAreaMap and CountryMap?
nrow(metro_area_map)
nrow(country_map)

#Merge CPS data frame with metro area map and country code
CPS = merge(cps, metro_area_map, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
sort(tapply(cps$Hispanic, cps$MetroArea, mean))

#determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

#determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))






