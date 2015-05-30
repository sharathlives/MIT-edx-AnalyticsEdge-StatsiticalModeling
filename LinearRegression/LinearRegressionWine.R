#Load the wine dataset
wine <- read.csv("wine.csv")

#Create a linear model
model1 <- lm(Price ~ HarvestRain+WinterRain, data = wine)

#Model with all variables
model2 <- lm(Price~ ., data = wine)

#New model without population
model3 <- lm( Price ~ AGST + HarvestRain+WinterRain+Age, data = wine)

#Compute corelation between age and France population
cor(wine$Age, wine$FrancePop)
cor(wine)

#Making Predictions
wineTest <- read.csv("wine_test.csv")
predictTest <- predict(model3, newdata = wineTest)
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
R_squared <- 1 - SSE/SST