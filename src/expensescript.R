rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
setwd("C:/Users/eung.cho/Desktop/legdays/travelExpenseReport")
source("src/getData.r")
source("src/enrichData.r")
source("src/summarizeData.r")

# Import data!
myRawData <- getCashTrailsData("data/CashTrails-20141015_1737.csv")
myItinerary <- getItinerary("data/destinations.csv")
myFXRates <- getExchangeRates("data/fxrates.csv", unique(myRawData$Currency))

myFormattedData <- formatCashTrailsData(myRawData)
myEnrichedCTData <- enrichCTData(myFormattedData, myItinerary, myFXRates)

myCountryDayCountTable <- getCountryDayCountTable(myItinerary)

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
