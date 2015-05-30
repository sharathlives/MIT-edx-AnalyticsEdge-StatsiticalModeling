#Load and clean the data
parole <- read.csv("parole.csv")

#Factorize the data
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

#Exploratory Analysis
table(parole$male, parole$violator) #14/78
table(parole$state, parole$crime) #drug related crimes are most common in Kentucky

#Create a histogram shwoing the distribution of age of the parolees
ggplot(data = parole, aes(x = parole$age)) +geom_histogram() #by default it is divided into 30 bins
ggplot(data = parole, aes(x = parole$age)) +geom_histogram(binwidth = 5) #binwidths divides into age brackets of 5

#Change the outline to blue
ggplot(data = parole, aes(x = parole$age)) +geom_histogram(binwidth = 5, color = "blue")

#Create a histogram for men and women using facet_grid
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)

#Put the histograms side by side
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)

#Add another dimension to the histograms with single aggregated histogram
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)

#Define color palette
colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5) + scale_fill_manual(values=colorPalette)

#Make transparent and dont stack one above the other
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) + scale_fill_manual(values=colorPalette)

#Create a basic histogram like the one we created in Problem 2, but this time with time.served on the x-axis. Set the bin width to one month.
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1)
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1) + facet_grid(crime ~ .)
ggplot(data=parole, aes(x=time.served, fill=crime)) + geom_histogram(binwidth=1, position="identity", alpha=0.5)

