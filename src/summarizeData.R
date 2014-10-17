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

constructTagCountryExpensePerDay <- function(aCTData, aCountryDayCountTable) {
  myTags <- unique(aCTData$Tag)
  myCountries <- as.character(unique(aCTData$Country))
  
  myTagCount <- length(myTags)
  myCountryCount <- length(myCountries)
  myTagCountryPairCount <- myTagCount * myCountryCount
  
  theTagCountryExpensePerDay <- data.frame(rep(myCountries, each=myTagCount), rep(myTags, myCountryCount), rep(0, myTagCountryPairCount))
  names(theTagCountryExpensePerDay) <- c("Country", "Tag", "StandardizedAmount")
  
  theTagCountryExpensePerDay["StandardizedAmount"] <- apply(theTagCountryExpensePerDay, 1, function(x) getTagCountryExpensePerDay(x[2], x[1], aCTData, aCountryDayCountTable))
  
  return(theTagCountryExpensePerDay)
}

getCumSumAtMoment <- function(aTime, aTagCumSum) {
  myIsTime <- aTagCumSum$UNIXTime <= aTime
  if(any(myIsTime)){
    theCumSumAtMoment <- max(aTagCumSum$CumSum[myIsTime])
  } else {
    theCumSumAtMoment <- 0
  }
  return(theCumSumAtMoment)
}


getFullTimeTagCumSum <- function(aTag, aTagCTData, aTimes) {
  # Assuming sorted
  myTagCumSum <- data.frame(aTagCTData$UNIXTime, cumsum(aTagCTData$StandardizedAmount))
  names(myTagCumSum) <- c("UNIXTime", "CumSum")
  theFullTimeTagCumSum <- sapply(aTimes, function(x) getCumSumAtMoment(x, myTagCumSum))
  return(theFullTimeTagCumSum)
}

constructTagCumSumPerTime <- function(aCTData) {
  myTimes <- unique(aCTData$UNIXTime)
  # myTags <- unique(aCTData$Tag)
  # order of stacking from top to bottom
  myTags <- rev(c("", "Food", "Accommodation", "Transit", "ATM", "Sim", "Massage", "Tip", "Gift",
    "Supplies", "Gas", "Laundry", "Internet", "Bathroom", "Gym", "Alcohol", "Gambling",
    "Taxi", "Bus", "Fees", "Rental", "Entry", "Tour"))
  theAllTagCumSumFullTime <- data.frame()
  
  for (aTag in myTags) {
    myIsTag <- aCTData$Tag == aTag
    myTagCTData <- aCTData[myIsTag,]
    myTagCumSumFullTime <- data.frame(myTimes, rep(aTag, length(myTimes)), getFullTimeTagCumSum(aTag, myTagCTData, myTimes))
    names(myTagCumSumFullTime) <- c("UNIXTime", "Tag", "TagCumSum")
    theAllTagCumSumFullTime <- rbind (myTagCumSumFullTime, theAllTagCumSumFullTime)
  }
  return(theAllTagCumSumFullTime)
}