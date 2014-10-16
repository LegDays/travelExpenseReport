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
  return(theCountryDayCountTable)
}