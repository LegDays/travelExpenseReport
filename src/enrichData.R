enrichCTData <- function(aCTData, aItinerary, aFXRates, aTimeBucket = "minute") {
#  expandAccommodationSpending(aCTData)
  aCTData <- aCTData[with(aCTData, order(aCTData$Date, aCTData$Time)), ]
  
  aCTData["Country"] <- getCountryColumn(aCTData, aItinerary)
  aCTData["Category"] <- getCategoryColumn(aCTData)
  aCTData["StandardizedAmount"] <- getStandardizedAmountColumn(aCTData, aFXRates)
  aCTData["TagCumSum"] <- getTagCumSumColumn(aCTData)
  aCTData["UNIXTime"] <- getUNIXTimeColumn(aCTData, aTimeBucket)
  
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

assignCategory <- function(aTag) {
  myLuxuryTags <- c("Massage", "Alcohol", "Gambling", "Tip", "Gift")
  myTourismTags <- c("Tour", "Rental", "Entry", "Fees")
  myFoodTags <- c("Food")
  myAccommodationTags <- c("Accommodation")
  myDayToDayTags <- c("Supplies", "Gym", "Internet", "Laundry", "Bathroom", "ATM", "Transit")
  myTaxiTags <- c("Taxi")
  myTravelTags <- c("Plane", "Gas", "Bus")

  #Right now, just returning tag
  if(aTag %in% myLuxuryTags) {
    theTag = "Luxury"
  } else if(aTag %in% myTourismTags) {
    theTag = "Tourism"
  } else if(aTag %in% myFoodTags) {
    theTag = "Food"
  } else if(aTag %in% myAccommodationTags) {
    theTag = "Accommodation"
  } else if(aTag %in% myDayToDayTags) {
    theTag = "DayToDay"
  } else if(aTag %in% myTaxiTags) {
    theTag = "Taxi"
  } else if(aTag %in% myTravelTags) {
    theTag = "Travel"
  } else {
    theTag = aTag
  }
  return(theTag)
}

getCategoryColumn <- function(aCTData) {
  theCountryColumn <- apply(aCTData, 1, function(x) assignCategory(x[5]))
  return(theCountryColumn)
}

getStandardizedAmountColumn <- function(aCTData, aFXRates) {
  theStandardizedAmountColumn <- mapply(function(c, a) aFXRates$Rate[aFXRates$Currency == c] * a,
                                        aCTData$Currency, aCTData$Amount)
  return(theStandardizedAmountColumn)
}

getTagCumSums <- function(aCTData, aTag) {
  # aCTData must be sorted
  myIsTag <- aCTData$Tag == aTag
  theTagCumSums <- cumsum(aCTData$StandardizedAmount[myIsTag])
  return(theTagCumSums)
}

getTagCumSumColumn <- function(aCTData) {
  myTags <- unique(aCTData$Tag)
  theTagCumSumColumn <- rep(9e9, dim(aCTData)[1])
  for (myTag in myTags) {
    myIsTag <- aCTData$Tag == myTag
    theTagCumSumColumn[myIsTag] <- getTagCumSums(aCTData, myTag)
  }
  return(theTagCumSumColumn)
}

getUNIXTimeColumn <- function(aCTData, aTimeBucket) {
  if(aTimeBucket == "minute") {
    theUNIXTimeColumn <- mapply(function(x, y) as.numeric(as.POSIXct(paste(x, y), format="%Y-%m-%d %H:%M")), aCTData$Date, aCTData$Time)
  } else if(aTimeBucket == "day") {
    theUNIXTimeColumn <- sapply(aCTData$Date, function(x) as.numeric(as.POSIXct(x, format="%Y-%m-%d")))
  } else {
    stop("aTimeBucket must be 'minute' or 'day'")
  }
  return(theUNIXTimeColumn)
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
