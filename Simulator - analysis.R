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

library('zoo')              # zoo package is loaded; it must be installed before running the script

spotsN      <- 100          # Number of sampling points in which you interpolate from all the time series to build a new averaged one
sensSd      <- 2            # Simulations whose duration is farther than "sensSd" Standard Deviation from the Mean won't be considered
choicePlaceName  <- ""  		# The places you're interested in; as default, all the places are considered
inputPath   <- "Results/"   # by default, input files are read from this folder inside the working directory
outputPath	<- "Analysis/"  # by default, output files will be placed in this folder inside the working directory
outputFile1	<- paste (outputPath, "finalmarkings.txt", sep = "")
outputFile2	<- paste (outputPath, "timeseries.txt", sep = "")
fileNames   <- list.files(inputPath)
iterNumber  <- length(fileNames)


# each matrix retrieved from a file is assigned to a specific variable and its final time is stored in a vector
if (iterNumber == 1) {
  inputFile <- paste(inputPath, fileNames, sep = "")
  tempInput <- read.table(inputFile, header = TRUE, sep="\t")
  assign("simulcoreOutput1",tempInput)
  maxTime = minTime  <- abs(tempInput[nrow(tempInput),ncol(tempInput)])
  valMat    <- 1

# the script changes a little when dealing with more than one input files (a for loop is required)
} else {
  maxTime     <- 0
  for (iterCounter in 1:iterNumber) {
    inputFile <- paste(inputPath,"simulation", iterCounter,".txt", sep = "")
    tempInput <- read.table(inputFile, header = TRUE, sep="\t")
    nam       <- paste ("simulcoreOutput",iterCounter, sep="")
    assign(nam,tempInput)
    maxTime[iterCounter]  <- abs(tempInput[nrow(tempInput),ncol(tempInput)])
  }
  
  # mean value and stardard deviation of the simulation durations are calculated
  meanTime <- mean(maxTime)
  sdTime   <- sd(maxTime)
  # simulation durations are plotted in a graph indicating mean value and 1x, 2x, 3x sd distances from the mean
  hist(maxTime, col="red")
  abline (v=c(meanTime, (meanTime - 2*sdTime), (meanTime + 2*sdTime)),col = "blue")
  abline (v=c((meanTime - sdTime), (meanTime+sdTime), (meanTime + 3*sdTime), (meanTime - 3*sdTime)),col="green")
  readline("\n Distribution of the duration of each iteration; press enter to continue")
  valMat = allMat   <- c(1:iterNumber)
  
  # simulation that lasted too differently from the mean are flagged; if a simulation has been flagged, it is removed from the pool
  invMat   <- c(which(maxTime > (meanTime+sensSd*sdTime)), which(maxTime < (meanTime -sensSd*sdTime)))   
  if (length(invMat) != 0) {valMat <- allMat[-invMat]}                                                   
  if (length(valMat) == 0) {valMat <- which.min(abs(maxTime - meanTime))}
  #the shortest simulation determines the duration of the mean time series
  minTime  <- min(maxTime[valMat])
  cat (100*length(invMat)/length(allMat), "% of the iteration ignored")
}

# other simulations are removed if the reached a dead state in the very first stage of the simulation
bornDead <- which(maxTime == 0)
if (length(bornDead) != 0) {
  valMat = valMat[-bornDead]
  cat ("\n One or more iteration ignored (the simulation reached a dead state at the very beginning)")
  if (length(valMat) == 0) {
    stop ("All the transitions have been ignored. No data available to calculate a time series")
  }
}

# this part is needed if you are interested in a specific place and have specified it at the beginning of the script
kplaceN         <- ncol(simulcoreOutput1) - 1
placesNames     <- colnames(simulcoreOutput1)[-(kplaceN+1)]
if (all(choicePlaceName == "")) {
  choicePlaceNumber <- c(1:kplaceN)
} else 	choicePlaceNumber <- which (colnames(simulcoreOutput1) == choicePlaceName)


####################################
### HERE STARTS THE ANALYSIS PART
####################################

dir.create ("Analysis", showWarnings = FALSE)

tableGlobal	   <- matrix(ncol= kplaceN+1, nrow= length(valMat))	# Table to summarize the results of each simulation (tokens in each place)
colnames(tableGlobal)	<- c(placesNames, "Dead State?")
totalOutput    <- matrix(ncol= kplaceN, nrow=(spotsN+1))        # Table to mean amount of tokens at each time point, i.e. the mean of all the simulations
totalOutput[,] <- 0 

for (matCounter in valMat) {
  timeLine      <- seq(0.0, minTime, length.out = spotsN + 1)   # time line created by subsetting the selected total duration for the chose number of time points.
  timeAxis      <- zoo(0, timeLine)
  nam2          <- paste("simulcoreOutput", matCounter, sep = "")
  assign("tempInput2",get(nam2))
  timeSeries	  <- zoo(tempInput2[,c(1:kplaceN)], tempInput2[,(kplaceN+1)])  #the original irregular time series is converted into a zoo object
  #aggregate(timeSeries, index(timeSeries), mean)                           # Extremely slow step; to be used only if you suspect that a transition happens so quickly that R cannot measure its duration
  
  # the original irregular time series is interpolated at specific time points, thus creating a regular time series
  mergedSeries  <- merge(timeSeries,timeAxis)
  mergedSeries[,c(1:kplaceN)] <- na.approx(mergedSeries[,c(1:kplaceN)], rule=2)
  timeIndex 	  <- which (index(mergedSeries) %in% index(timeAxis))
  simulcoreTemp <- as.matrix(mergedSeries[timeIndex,c(1:kplaceN)])
  # the regular time series can be summed to the others because it has been created by interpolating in the same time points
  totalOutput   <- totalOutput + simulcoreTemp[c(1:(spotsN+1)),]
  
  # the results of each simulation, modified during the interpolation phase, are recorded into the summarizing table
  if (tempInput[nrow(tempInput),kplaceN+1] < 0) {
    tableGlobal[which(valMat == matCounter),]<- c(round(simulcoreTemp[spotsN+1,],3), "YES")
  } else  tableGlobal[which(valMat == matCounter),]<- c(round(simulcoreTemp[spotsN+1,],3), "NO")
}

# the actual mean is calculated in the mean matrix by diving the total amount of tokens for the number of simulations
if (length(valMat) > 1) { # there's no need to do that if only one simulation has been analysed.
  totalOutput <- totalOutput/length(valMat)
  tableMean		<- totalOutput[spotsN+1,]
  tableGlobal	<- rbind(tableGlobal, c(round(tableMean,3), ""))
  rownames(tableGlobal)		<- c(valMat, "Mean")
}

# a graph is plotted, showing the amount of tokens during all the simulation; a specific place or all the places are plotted according to the first settings.
if (choicePlaceName != "") {
  cat ("\n therefore, the (mean) value of ", choicePlaceName, " is ", tableMean[choicePlaceNumber], "for each iteration")
  plot ((0:spotsN), totalOutput[,choicePlaceNumber], type="l")
} else	{
  matplot ((0:spotsN), totalOutput, type="l")
  legend('topright', placesNames , col=1:6, lty=1, bty='n', cex=.75)
}

# the mean table and the summarizing table are exported into two text files
totalOutput <-cbind(totalOutput, timeLine)
write.table (tableGlobal, outputFile1, quote = FALSE, sep = "\t",row.names = FALSE, )  #print the summary table and the mean value of the selected place
write.table (round(totalOutput,3), outputFile2, quote = FALSE, sep = "\t",row.names = FALSE, )  #print the summary table and the mean value of the selected place