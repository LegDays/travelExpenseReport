rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
setwd("C:/Users/eung.cho/Desktop/legdays/travelExpenseReport")

library(rCharts)
library(rjson)
library(ggplot2)
source("src/getData.r")
source("src/enrichData.r")
source("src/filterData.r")
source("src/summarizeData.r")
source("src/plotData.r")

# Import data!
myItinerary <- getItinerary("data/destinations.csv")
myCountryDayCountTable <- getCountryDayCountTable(myItinerary)
myRawData <- getCashTrailsData("data/CashTrails-20141016_1720.csv")
myFXRates <- getExchangeRates("data/fxrates.csv", unique(myRawData$Currency))

myEnrichedCTData <- enrichCTData(myRawData, myItinerary, myFXRates)
# Check border transactions
# myEnrichedCTData[is.na(myEnrichedCTData$Country),]
myFilteredCTData <- filterUnusualExpenses(myEnrichedCTData)

myTagCountryExpensePerDay <- constructTagCountryExpensePerDay(myFilteredCTData, myCountryDayCountTable)

myTagCumSumPerTime <- constructTagCumSumPerTime(myFilteredCTData)
ggplot(myTagCumSumPerTime, aes(x=UNIXTime,y=TagCumSum,group=Tag,fill=Tag)) + geom_area(position="stack")
p2 <- nPlot(TagCumSum ~ UNIXTime, group = 'Tag', data = myTagCumSumPerTime, type = 'stackedAreaChart')
p2

plot(myEnrichedCTData$Date, cumsum(myEnrichedCTData$StandardizedAmount), type='h',
     col = as.factor(myEnrichedCTData$Country))

