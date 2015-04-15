####################################################################################
#
# stock_analysis_Part2.R contains the solution script for the second exercise of day
#  3 for the Data Science Intro course for DU
# Written by Paul Fornia. March, 2015 
#
####################################################################################


library(ggplot2)

main <- function(){
  ## Read in data
  mydata <- read.csv("./stock.csv",
                     stringsAsFactors=FALSE)
  
  ## Change time to correct format
  mydata$time <- as.POSIXct(mydata$time, format="%H:%M:%S")
  
  ## Only need data between 9:30AM and 9:45
  startTime <- as.POSIXct("9:30:00", format="%H:%M:%S")
  endTime <- as.POSIXct("9:45:00", format="%H:%M:%S")

  mydata <- mydata[(mydata$time > startTime),]
  mydata <- mydata[(mydata$time < endTime),]
  mydata <- mydata[(mydata$date == "3/12/2015"),]
  
  ## Select subset of data: time and volatility, time and delta
  vola <- mydata[c(2,7,8)]
   
  startTime <- vola$time[1]
  endTime <- vola$time[length(vola$time)]
  elapseTimeSec <- as.numeric(endTime - startTime) * 60
  dim(vola)
 
  ## Create template MA table
  volaMA <- vola[1:(elapseTimeSec - 60),]
  
  ## For 1-min moving average, remove the last 60 sec
  for(i in 1:(elapseTimeSec-60)){
    curTime <- startTime + i +30
    startWindow <- curTime - 30
    endWindow <- curTime + 30
    
    volaAfter <- vola[vola$time > startWindow,]
    volaInWindow <- volaAfter[volaAfter$time < endWindow,]
    
    volaMA$time[i] <- as.POSIXct(curTime, format="%H:%M:%S")
    volaMA$volatility[i] <- mean(volaInWindow$volatility)
    volaMA$delta[i] <- mean(volaInWindow$delta)
    
  }

  ## Print the correlation
  
  print("Correlation between Volatility and Delta:")
  print(cor(vola$volatility, vola$delta))
  
  ## Plot volatility time series chart using ggplot  
  
  plot1 <- ggplot(data=volaMA, aes(x=time, y=volatility)) + geom_line()
  plot2 <- ggplot(data=volaMA, aes(x=time, y=delta)) + geom_line()
  
  list(volaPlot = plot1, deltaPlot = plot2)
}
