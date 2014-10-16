rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
source("getData.r")

# Import data!
setwd("C:/Users/eung.cho/Desktop/legdays/travelExpenseReport")
myRawData <- getCashTrailsData("data/CashTrails-20141015_1737.csv")
myArrivalDepartures <- getItinerary("data/destinations.csv")

allocateAccommodationSpendingAndSort <- function(aData) {
  # Allocate accomodation spending
  myAccommData <- subset(aData, aData$Tags=="Accommodation")
  myNonAccommData <- subset(aData, !aData$Tags=="Accommodation")
  myAccommData$Note <- sapply(myAccommData$Note, function(x) if (is.na(as.numeric(x))) "1" else x)
  myAllocAccomData<- myAccommData[which(is.na(myAccommData$Amount)), ]
  allocateOutAccom <- function(aDataFrameRow) {
    myDate <- aDataFrameRow$Date
    myTime <- aDataFrameRow$Time
    myNights <- as.numeric(aDataFrameRow$Note)
    myCost <- aDataFrameRow$Amount
    myCurrency <- aDataFrameRow$Currency
    myTags <- aDataFrameRow$Tags
    myPerNightRate <- myCost / myNights
    
    theAllocRows <- data.frame(Date=character(myNights), Time=character(myNights), Amount=numeric(myNights), 
                               Currency=character(myNights), Tags=character(myNights), Note=character(myNights))
    theAllocRows$Date <- rep(myDate) + 0:(myNights-1)
    theAllocRows$Time <- rep(myTime,myNights)
    theAllocRows$Amount <- rep(myPerNightRate,myNights)
    theAllocRows$Currency <- rep(myCurrency,myNights)
    theAllocRows$Tags <- rep(myTags,myNights)
    theAllocRows$Note <- rep("1",myNights)
    
    return(theAllocRows)
  }
  for (i in 1:dim(myAccommData)[1]) {
    myAllocatedRows <- allocateOutAccom(myAccommData[i,])
    myAllocAccomData <- rbind(myAllocAccomData, myAllocatedRows)
    print(i/dim(myAccommData)[1])
  }
  theCombinedData<-rbind(myAllocAccomData,myNonAccommData)
  theCombinedData <- theCombinedData[with(theCombinedData, order(theCombinedData$Date, theCombinedData$Time)), ]
  return(theCombinedData)
}

assignCountry <- function(aRow,aArrivalDepartures) {
  myDate<-as.Date(aRow[1])
  myCurrency <-aRow[4]
  myIsInDateRange <- mapply(function(d,a) d <= myDate && myDate <= a,aArrivalDepartures$Arrival, aArrivalDepartures$Departure)
  if (sum(myIsInDateRange) == 1) {
    theCountry <- aArrivalDepartures[myIsInDateRange,]$Country
  } else if (sum(myIsInDateRange) > 1) {
    myIsCurrency <- aArrivalDepartures$Currency == myCurrency
    theCountry<- aArrivalDepartures$Country[myIsInDateRange & myIsCurrency][1]
  }
  return(theCountry)
}
assignCountryColumn <- function(aData, aArrivalDepartures) {
  
  theCountryColumn <- apply(aData, 1, function(x) assignCountry(x, aArrivalDepartures))
  return(theCountryColumn)
}
getCountryDayCount <- function(aCountry, aArrivalDepartures) {
  myIsCountry<-aArrivalDepartures$Country==aCountry
  myArrivals<-aArrivalDepartures$Arrival[myIsCountry]
  myDepartures<-aArrivalDepartures$Departure[myIsCountry]
  myDepartures - myArrivals + 1
}

myFormattedData <- formatCashTrailsData(myRawData)
myCleanData <- allocateAccommodationSpendingAndSort(myFormattedData)
myCurrencyExchangeRates <- getExchangeRateTable("fxrates.csv", unique(aCashTrailsData$Currency))
myCleanData["Country"] <- assignCountryColumn(myCleanData,myArrivalDepartures)

myCountries <- unique(myCleanData["Country"])
myCountriesDayCounts<-
sapply(myCountries, function(x) )

# Standardize amonuts to USD
myCleanData$StandardizedAmount <- mapply(function(aCurrency, aAmount)
                                      myCurrencyExchangeRates$Rate[myCurrencyExchangeRates$Currency == aCurrency] * aAmount,
                                      myCleanData$Currency, myCleanData$Amount)

myCumSum<-cumsum(myCleanData$StandardizedAmount)
plot( myCleanData$Date,myCumSum, type='h', col = as.factor(myCleanData$Country))

myCleanDataNoFlights <- subset(myCleanData, myCleanData$Tags!="Plane" & myCleanData$Tags!="Supplies" & myCleanData$Tags!="Misc" & !(myCleanData$Currency %in% c("MYR", "AED")))
myCumSumNoFlights<-cumsum(myCleanDataNoFlights$StandardizedAmount)
palette(rainbow(15))
plot( myCleanDataNoFlights$Date, myCumSumNoFlights, type='h', col = as.factor(myCleanDataNoFlights$Currency))

myCurrencies <- unique(myCleanDataNoFlights$Currency)
myPerAmounts <- rep(0, length(myCurrencies))
myCurrencyDayCounts <- data.frame(rep(0, length(myCurrencies)), myCurrencies)
for (i in 1:length(myCurrencies)) {
  
  myCurrency <- myCurrencies[i]
  myIsCurrency <- myCleanDataNoFlights$Currency==myCurrency
  myDayCount <- length(unique(myCleanDataNoFlights$Date[myIsCurrency]))
  myTotal <- sum(myCleanDataNoFlights$StandardizedAmount[myIsCurrency])
  myPerAmounts[i] <- myTotal / myDayCount
  myCurrencyDayCounts[i,1] <- myDayCount
  
}

myPerCurrencyBreak <- data.frame(myCurrencies, myPerAmounts)
myPerCurrencyBreak <- myPerCurrencyBreak[with(myPerCurrencyBreak, order(myPerCurrencyBreak$myPerAmounts)), ]
myPerCurrencyBreak$myCurrencies <- factor(myPerCurrencyBreak$myCurrencies, levels = myPerCurrencyBreak$myCurrencies)
plot(myPerCurrencyBreak, type = 'h')
barplot(myPerCurrencyBreak$myPerAmounts, names=myPerCurrencyBreak$myCurrencies,
        col = heat.colors(length(unique(myPerCurrencyBreak$myPerAmounts)))[as.factor(myPerCurrencyBreak$myPerAmounts)])

myTagCurrencySums <- tapply(myCleanDataNoFlights$StandardizedAmount, list(myCleanDataNoFlights$Tags,myCleanDataNoFlights$Currency), sum)
myTagCurrencySums[is.na(myTagCurrencySums)]<-0

myPerTagPerDayBreak <- sapply(colnames(myTagCurrencySums), function (x) myTagCurrencySums[,x]/myCurrencyDayCounts[myCurrencyDayCounts$myCurrencies==x,1])
myPerDayBreak <- apply(myPerTagPerDayBreak, 2, sum)
myPerDayBreak <- myPerDayBreak[order(myPerDayBreak)]
barplot(myPerDayBreak, names = names(myPerDayBreak), col = heat.colors(length(unique(names(myPerDayBreak)))))

myPerTagPerDayBreak<-myPerTagPerDayBreak[,names(myPerDayBreak)]

barplot(myPerTagPerDayBreak, col = rainbow(20), border = NA)
