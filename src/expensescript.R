rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
setwd("C:/Users/eung.cho/Desktop/legdays/travelExpenseReport")

source("src/getData.r")
source("src/enrichData.r")
source("src/filterData.r")
source("src/summarizeData.r")
source("src/plotData.r")

# Import data!
myItinerary <- getItinerary("data/destinations.csv")
myRawData <- getCashTrailsData("data/CashTrails-20141015_1737.csv")
myFXRates <- getExchangeRates("data/fxrates.csv", unique(myRawData$Currency))

myEnrichedCTData <- enrichCTData(myRawData, myItinerary, myFXRates)
# Check border transactions
myEnrichedCTData[is.na(myEnrichedCTData$Country),]
myFilteredCTData <- filterUnusualExpenses(myEnrichedCTData)

myCountryDayCountTable <- getCountryDayCountTable(myItinerary)

myTagCountryExpensePerDayMatrix <- constructTagCountryExpensePerDayMatrix(myFilteredCTData, myCountryDayCountTable)

barplot(myTagCountryExpensePerDayMatrix, col = rainbow(20))

plot(myEnrichedCTData$Date, cumsum(myEnrichedCTData$StandardizedAmount), type='h',
     col = as.factor(myEnrichedCTData$Country))
