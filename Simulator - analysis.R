### Copyright 2014 Lorenzo Ficorella
###
### This program is free software: you can redistribute it and/or modify
### it under the terms of the GNU Lesser General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU Lesser General Public License for more details.
###
### You should have received a copy of the GNU Lesser General Public License
### along with this program.  If not, see <http://www.gnu.org/licenses/>.


####################################
### HERE STARTS THE INPUT PART
####################################

library('zoo')

spotsN      <- 100          # Number of sampling points in which you interpolate from all the time series to build a new averaged one
sensSd      <- 2            # Simulations whose duration is farther than "sensSd" Standard Deviation from the Mean won't be considered
choicePlaceName  <- ""  		# The places you're interested in; as default, all the places are considered
inputPath   <- "Results/"
outputPath	<- "Analysis/"
outputFile1	<- paste (outputPath, "finalmarkings.txt", sep = "")
outputFile2	<- paste (outputPath, "timeseries.txt", sep = "")
fileNames   <- list.files(inputPath)
iterNumber  <- length(fileNames)


if (iterNumber == 1) {
  inputFile <- paste(inputPath, fileNames, sep = "")
  tempInput <- read.table(inputFile, header = TRUE)
  assign("simulcoreOutput1",tempInput)
  maxTime = minTime  <- abs(tempInput[nrow(tempInput),ncol(tempInput)])
  valMat    <- 1
  
} else {
  maxTime     <- 0
  for (iterCounter in 1:iterNumber) {
    inputFile <- paste(inputPath,"simulation", iterCounter,".txt", sep = "")
    tempInput <- read.table(inputFile, header = TRUE)
    nam       <- paste ("simulcoreOutput",iterCounter, sep="")
    assign(nam,tempInput)
    maxTime[iterCounter]  <- abs(tempInput[nrow(tempInput),ncol(tempInput)])
  }
  
  meanTime <- mean(maxTime)
  sdTime   <- sd(maxTime)
  hist(maxTime, col="red")
  abline (v=c(meanTime, (meanTime - 2*sdTime), (meanTime + 2*sdTime)),col = "blue")
  abline (v=c((meanTime - sdTime), (meanTime+sdTime), (meanTime + 3*sdTime), (meanTime - 3*sdTime)),col="green")
  readline("\n Distribution of the duration of each iteration; press enter to continue")
  valMat = allMat   <- c(1:iterNumber)
  invMat   <- c(which(maxTime > (meanTime+sensSd*sdTime)), which(maxTime < (meanTime -sensSd*sdTime)))
  if (length(invMat) != 0) {valMat <- allMat[-invMat]}
  if (length(valMat) == 0) {valMat <- which.min(abs(maxTime - meanTime))}
  minTime  <- min(maxTime[valMat])
  cat (100*length(invMat)/length(allMat), "% of the iteration ignored")
}


bornDead <- which(maxTime == 0)
if (length(bornDead) != 0) {
  valMat = valMat[-bornDead]
  cat ("\n One or more iteration ignored (the simulation reached a dead state at the very beginning)")
  if (length(valMat) == 0) {
    stop ("All the transitions have been ignored. No data available to calculate a time series")
  }
}

kplaceN         <- ncol(simulcoreOutput1) - 1
placesNames     <- colnames(simulcoreOutput1)[-(kplaceN+1)]
if (all(choicePlaceName == "")) {
  choicePlaceNumber <- c(1:kplaceN)
} else 	choicePlaceNumber <- which (colnames(simulcoreOutput1) == choicePlaceName)


####################################
### HERE STARTS THE ANALYSIS PART
####################################

dir.create ("Analysis", showWarnings = FALSE)

tableGlobal	   <- matrix(ncol= kplaceN+1, nrow= length(valMat))	# Table to summarize the results of all runs and the mean value of the runs (for each place)
colnames(tableGlobal)	<- c(placesNames, "Dead State?")
totalOutput    <- matrix(ncol= kplaceN, nrow=(spotsN+1))
totalOutput[,] <- 0 

for (matCounter in valMat) {
  timeLine      <- seq(0.0, minTime, length.out = spotsN + 1)
  timeAxis      <- zoo(0, timeLine)
  nam2          <- paste("simulcoreOutput", matCounter, sep = "")
  assign("tempInput2",get(nam2))
  timeSeries	  <- zoo(tempInput2[,c(1:kplaceN)], tempInput2[,(kplaceN+1)])
  #aggregate(timeSeries, index(timeSeries), mean)              # Only useful if a transition happens so quickly that R cannot measure its duration
  
  mergedSeries  <- merge(timeSeries,timeAxis)
  mergedSeries[,c(1:kplaceN)] <- na.approx(mergedSeries[,c(1:kplaceN)], rule=2)
  timeIndex 	  <- which (index(mergedSeries) %in% index(timeAxis))
  simulcoreTemp <- as.matrix(mergedSeries[timeIndex,c(1:kplaceN)])
  totalOutput   <- totalOutput + simulcoreTemp[c(1:(spotsN+1)),]
  
  if (tempInput[nrow(tempInput),kplaceN+1] < 0) {
    tableGlobal[which(valMat == matCounter),]<- c(round(simulcoreTemp[spotsN+1,],3), "YES")
  } else  tableGlobal[which(valMat == matCounter),]<- c(round(simulcoreTemp[spotsN+1,],3), "NO")
}


if (length(valMat) > 1) {
  totalOutput <- totalOutput/length(valMat)
  tableMean		<- totalOutput[spotsN+1,]
  tableGlobal	<- rbind(tableGlobal, c(round(tableMean,3), ""))
  rownames(tableGlobal)		<- c(valMat, "Mean")
}

if (choicePlaceName != "") {
  cat ("\n therefore, the (mean) value of ", choicePlaceName, " is ", tableMean[choicePlaceNumber], "for each iteration")
  plot ((0:spotsN), totalOutput[,choicePlaceNumber], type="l")
} else	{
  matplot ((0:spotsN), totalOutput, type="l")
  legend('topright', placesNames , col=1:6, lty=1, bty='n', cex=.75)
}

totalOutput <-cbind(totalOutput, timeLine)
write.table (tableGlobal, outputFile1, quote = FALSE, sep = "\t",row.names = FALSE, )  #print the summary table and the mean value of the selected place
write.table (round(totalOutput,3), outputFile2, quote = FALSE, sep = "\t",row.names = FALSE, )  #print the summary table and the mean value of the selected place
