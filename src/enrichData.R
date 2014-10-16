enrichCTData <- function(aCTData, aItinerary, aFXRates) {
#  expandAccommodationSpending(aCTData)
  aCTData <- aCTData[with(aCTData, order(aCTData$Date, aCTData$Time)), ]
  
  aCTData["Country"] <- getCountryColumn(aCTData, aItinerary)
  aCTData["StandardizedAmount"] <- getStandardizedAmountColumn(aCTData, aFXRates)
  return(aCTData)
}

assignCountry <- function(aDate, aCurrency, aItinerary) {
  myIsInDateRange <- mapply(function(d,a) d <= aDate && aDate <= a,aItinerary$Arrival, aItinerary$Departure)
  if (sum(myIsInDateRange) == 1) {
    theCountry <- aItinerary[myIsInDateRange,]$Country
  } else if (sum(myIsInDateRange) > 1) {
    myIsCurrency <- aItinerary$Currency == aCurrency
    theCountry <- aItinerary$Country[myIsInDateRange & myIsCurrency][1]
  } else {
    theCountry <- NA
  }
  return(theCountry)
}

getCountryColumn <- function(aCTData, aItinerary) {
  theCountryColumn <- apply(aCTData, 1, function(x) assignCountry(x[1], x[4], aItinerary))
  return(theCountryColumn)
}

getStandardizedAmountColumn <- function(aCTData, aFXRates) {
  theStandardizedAmountColumn <- mapply(function(c, a) aFXRates$Rate[aFXRates$Currency == c] * a,
                                        aCTData$Currency, aCTData$Amount)
  return(theStandardizedAmountColumn)
}

expandAccommodationSpending <- function(aData) {
  # Allocate accomodation spending
  myAccommData <- subset(aData, aData$Tag=="Accommodation")
  myNonAccommData <- subset(aData, !aData$Tag=="Accommodation")
  myAccommData$Note <- sapply(myAccommData$Note, function(x) if (is.na(as.numeric(x))) "1" else x)
  myAllocAccomData<- myAccommData[which(is.na(myAccommData$Amount)), ]
  allocateOutAccom <- function(aDataFrameRow) {
    myDate <- aDataFrameRow$Date
    myTime <- aDataFrameRow$Time
    myNights <- as.numeric(aDataFrameRow$Note)
    myCost <- aDataFrameRow$Amount
    myCurrency <- aDataFrameRow$Currency
    myTags <- aDataFrameRow$Tag
    myPerNightRate <- myCost / myNights
    
    theAllocRows <- data.frame(Date=character(myNights), Time=character(myNights), Amount=numeric(myNights), 
                               Currency=character(myNights), Tag=character(myNights), Note=character(myNights))
    theAllocRows$Date <- rep(myDate) + 0:(myNights-1)
    theAllocRows$Time <- rep(myTime,myNights)
    theAllocRows$Amount <- rep(myPerNightRate,myNights)
    theAllocRows$Currency <- rep(myCurrency,myNights)
    theAllocRows$Tag <- rep(myTags,myNights)
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
