filterUnusualExpenses <- function(aCTData) {
  myIsPlane <- aCTData$Tag == "Plane"
  myIsMisc <- aCTData$Tag == "Misc"
  myIsShortStay <- aCTData$Currency %in% c("MYR", "AED")
  myIsNACountry <- is.na(aCTData$Country)
  theFilteredCTData <- subset(aCTData, (!myIsPlane & !myIsMisc & !myIsShortStay & !myIsNACountry))
}