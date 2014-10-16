getCountryDayCount <- function(aCountry, aItinerary) {
  myIsCountry <- aItinerary$Country==aCountry
  myArrivals <- aItinerary$Arrival[myIsCountry]
  myDepartures <- aItinerary$Departure[myIsCountry]
  theCountryDayCount <- sum(myDepartures - myArrivals + 1)
  return(theCountryDayCount)
}

getCountryDayCountTable <- function(aItinerary) {
  myCountries <- unique(aItinerary$Country)
  myCountriesDayCounts<-
    sapply(myCountries, function(x) getCountryDayCount(x, aItinerary))
  theCountryDayCountTable <- data.frame(myCountries, myCountriesDayCounts)
  colnames(theCountryDayCountTable) <- c("Country", "DayCount")
  return(theCountryDayCountTable)
}

getExpensePerDay <- function(aExpenseTotal, aCountry, aCountryDayCountTable) {
  myIsCountry <- aCountryDayCountTable$Country == aCountry
  myDayCount <- aCountryDayCountTable$DayCount[myIsCountry]
  theExpensePerDay <- aExpenseTotal / myDayCount
  return(theExpensePerDay)
}

getTagCountryExpensePerDay <- function(aTag, aCountry, aCTData, aCountryDayCountTable) {
  myIsTag <- aCTData$Tag == aTag
  myIsCountry <- aCTData$Country == aCountry
  myTagCountryStandardizedAmount <- sum(aCTData$StandardizedAmount[myIsTag & myIsCountry])
  theTagCountryExpensePerDay <- getExpensePerDay(myTagCountryStandardizedAmount, aCountry, aCountryDayCountTable)
  return(theTagCountryExpensePerDay)
}

getTagCountryExpensePerDayColumn <- function(aTags, aCountries, aCTData, aCountryDayCountTable) {
  theTagCountryExpensePerDayColumn <- mapply(function(x, y) getTagCountryExpensePerDay(x, y, aCTData, aCountryDayCountTable), aTags, aCountries)
}

constructTagCountryExpensePerDayMatrix <- function(aCTData, aCountryDayCountTable) {
  myTags <- unique(aCTData$Tag)
  myCountries <- unique(aCTData$Country)

  theTagCountryExpensePerDayMatrix <- outer(myTags, myCountries, function(x, y) getTagCountryExpensePerDayColumn(x, y, aCTData, aCountryDayCountTable))
  
  rownames(theTagCountryExpensePerDayMatrix) <- myTags
  colnames(theTagCountryExpensePerDayMatrix) <- myCountries
  
  myCountryTotals <- apply(theTagCountryExpensePerDayMatrix, 2, sum)
  theTagCountryExpensePerDayMatrix<-theTagCountryExpensePerDayMatrix[,order(myCountryTotals)]
  
  return(theTagCountryExpensePerDayMatrix)
}