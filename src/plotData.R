if(FALSE) {
  titanic <- as.data.frame(myFilteredCTData)
  d1 <- dPlot(
    StandardizedAmount ~ Country,
    groups = "Tag",
    data = titanic,
    type = "bar"
  )
  d <- data.frame(t=rep(0:23,each=4),var=rep(LETTERS[1:4],4),val=round(runif(4*24,0,50)))
  
  myTagCumSumPerTime <- constructTagCumSumPerTime(myFilteredCTData)
  ggplot(myTagCumSumPerTime, aes(x=UNIXTime,y=TagCumSum,group=Tag,fill=Tag)) + geom_area(position="stack")
  
  
  data(economics, package = 'ggplot2')
  ecm <- reshape2::melt(economics[,c('date', 'uempmed', 'psavert')], id = 'date')
  p2 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'stackedAreaChart')
  p2
  
  p2 <- nPlot(TagCumSum ~ UNIXTime, group = 'Tag', data = myTagCumSumPerTime, type = 'stackedAreaChart')
  options(viewer=NULL)
  p2
  
  
  d1$set(width = 1200)
  d1
  
  titanic <- as.data.frame(myTagCountryExpensePerDay)
  d2 <- dPlot(
    StandardizedAmount ~ Country,
    groups = "Tag",
    data = titanic,
    type = "bar"
  )
  
  d2$set(width = 1200)
  d2
}