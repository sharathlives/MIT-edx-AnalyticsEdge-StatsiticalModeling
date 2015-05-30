#Read the files
ibm <- read.csv("IBMStock.csv")
ge <- read.csv("GEStock.csv")
cocacola <- read.csv("CocaColaStock.csv")
pg <- read.csv("ProcterGambleStock.csv")
boeing <- read.csv("BoeingStock.csv")

#Transform the date variable
ibm$Date = as.Date(ibm$Date, "%m/%d/%y")
ge$Date = as.Date(ge$Date, "%m/%d/%y")
cocacola$Date = as.Date(cocacola$Date, "%m/%d/%y")
pg$Date = as.Date(pg$Date, "%m/%d/%y")
boeing$Date = as.Date(boeing$Date, "%m/%d/%y")

#Around what year did Coca-Cola has its highest and loweststock price in this time period?
plot(cocacola$Date, cocacola$StockPrice)
cocacola$Date[which.min(cocacola$StockPrice)]

#let's add the line for Procter & Gamble 
plot(cocacola$Date, cocacola$StockPrice, col = "red")
lines(pg$Date, pg$StockPrice, col = "blue")

#In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more?
abline(v=as.Date(c("2000-03-01")), lwd=2)

#How did stock prices change between 1995 to 2005
#cocacola[cocacola$Date %in% as.Date(c('1995-01-01', '2005-01-01')),] returns just 2 dates
#cocacola_datefilter <- subset(cocacola, Date > as.Date("1995-01-01") & Date < as.Date("2005-12-31"))
plot(cocacola$Date[301:432], cocacola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(boeing$Date[301:432], boeing$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(boeing$Date[301:432], boeing$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))
lines(pg$Date[301:432], pg$StockPrice[301:432], type="l", col="green", ylim=c(0,210))
lines(ibm$Date[301:432], ibm$StockPrice[301:432], type="l", col="yellow", ylim=c(0,210))
lines(ge$Date[301:432], ge$StockPrice[301:432], type="l", col="black", ylim=c(0,210))
abline(v=as.Date(c("2000-03-01")), lwd=2, lty =4)

# Do stocks tend to be higher or lower during certain months
tapply(ibm$StockPrice, months(ibm$Date), mean)
mean(IBM$StockPrice)

