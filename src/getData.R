getCashTrailsData <- function(aFileName) {
  myRawCTData <- read.csv(aFileName)
  myCharacterCTData <- data.frame(lapply(myRawCTData, as.character), stringsAsFactors=FALSE)
  
  # Rename columns
  myOldColumns <- c("ï..Date","Time", "Amount...Withdrawal.Amount", "Currency.Code", "Tags", "Note")
  myNewColumns <- c("Date", "Time", "Amount", "Currency", "Tag", "Note")
  
  myRelevantCTData <- extractRelevantCashTrailsColumns(myCharacterCTData, myOldColumns)
  myRenamedCTData <- renameCashTrailsColumns(myRelevantCTData, myOldColumns, myNewColumns)
  theData <- formatCashTrailsData(myRenamedCTData)
  
  return(theData)
}

extractRelevantCashTrailsColumns <- function(aData, aColumnNames){
  theData<- aData[,(names(aData) %in% aColumnNames)]
  return(theData)
}

renameCashTrailsColumns <- function(aData, aOldColNames, aNewColNames) {
  for (i in 1:length(aOldColNames)) {
    names(aData)[names(aData)==aOldColNames[i]] <- aNewColNames[i]
  }
  return(aData)
}

formatCashTrailsData <- function(aData) {
  aData$Amount <- -as.numeric(aData$Amount)
  aData$Date <- as.Date(aData$Date)
  return(aData)
}

getItinerary<- function(aFileName) {
  myUnformattedItinerary <- read.csv(aFileName)
  theItinerary<-formatItinerary(myUnformattedItinerary)
  return(theItinerary)
}

formatItinerary <- function(aItinerary) {
  aItinerary$Arrival <- as.Date(aItinerary$Arrival)
  aItinerary$Departure <- as.Date(aItinerary$Departure)
  return(aItinerary)
}

getExchangeRates <- function(aFileName, aCurrencySymbols) {
  # Get FX rates!
  #http://www.oanda.com/currency/table
  myRawExchangeRates <- read.csv(aFileName)
  theExchangeRates = data.frame(aCurrencySymbols, rep(9e9,length(aCurrencySymbols)))
  colnames(theExchangeRates) <- c("Currency", "Rate")
  theExchangeRates$Rate <- sapply(theExchangeRates$Currency,
                                          function(x)
                                            myRawExchangeRates$USD.1.Unit[myRawExchangeRates$Code == as.character(x)])
  return(theExchangeRates)
}