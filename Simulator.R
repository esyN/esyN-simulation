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

stepsNumber	<- 100
iterNumber	<- 100
choicePlaceName	<- ""					# the places you're interested in; as default, all the places are considered
inputPath	<- ""
outputPath	<- ""
inputFileName	<- "merge_matrices.txt"
outputFileName	<- "finalmarkings.txt"


library('rjson')
library('zoo')
inputFile	<- paste (inputPath, inputFileName, sep = "")
inputData	<- fromJSON(paste(readLines(inputFile), collapse=""))
outputFile	<- paste (outputPath, outputFileName, sep = "")

matrixTokens 	<- inputData$marking 			# tokens in all the places at time 0 (i.e. the tokens you've written in the network)
transitNames	<- inputData$tnames
placesNames	<- inputData$pnames			# you'll need those variables later to run the simulation
kplaceN 	<- length(placesNames)			# Number of places
ktransitN 	<- length(transitNames) 		# Number of Transitions
matrixInhibit  	<- do.call(rbind, inputData$inhib)	# matrix of the weights of inhibitory arcs, always going FROM places TO transitions
inhibIndex 	<- which(matrixInhibit>=1, arr.ind=TRUE)#see where are inhibitions in the matrix
matrixInward 	<- do.call(rbind, inputData$post)
matrixOutward 	<- -1*(do.call(rbind, inputData$pre))
matrixDelta 	<- matrixInward + matrixOutward
colnames(matrixInhibit) <- colnames(matrixInward) <- colnames(matrixOutward) <- colnames(matrixDelta) <- placesNames

#vectorPar 	<- inputData$tparameters	# this feature is not available at the moment; an uniform vector will be provided
vectorPar	<- rep (1, ktransitN)

if (all(choicePlaceName == "")) {
	choicePlaceNumber <- c(1:kplaceN)
} else 	choicePlaceNumber <- which (colnames(matrixInward) == choicePlaceName)


####################################
### HERE STARTS THE CORE PART
####################################

Simulcore <- function() {     #the core of the simulation; each step is repeated for stepsNumber times (x=1 is the starting condition)
	matrixMatrix 	<- matrix(ncol =kplaceN+1, nrow= stepsNumber)
	matrixMatrix[1,]<- c(matrixTokens,0)
	totTime 	<- 0
	matrixRow	<- 1

	while (totTime < stepsNumber) {
		vectorTrans <- 1:ktransitN				
		tokenSum    <- rowSums(-1*sweep(matrixOutward,2,matrixTokens,`*`))
			if (!all(tokenSum == 0))	{tokenSum[which(tokenSum == 0)] = 1}
		vectorProb <- tokenSum*vectorPar
		vectorProb <- (-1/vectorProb)*log(runif(ktransitN,min=0, max=1))

		if (matrixRow == nrow(matrixMatrix)) {
			addingRows   <- matrix (ncol=kplaceN+1, nrow = stepsNumber)
			matrixMatrix <- rbind(matrixMatrix, addingRows)
		}
		repeat {
			parTime	<- min(vectorProb)
			chance	<- which.min(vectorProb)
			rn	<- vectorTrans[chance]
 			
			if (length(inhibIndex) == 0) {
				if (all(matrixTokens >= -matrixOutward[rn,])) {
					#the real transition is completed, adding tokens to postplaces and removing them from preplaces
  					matrixTokens 	<- matrixTokens + matrixDelta[rn,]
					totTime 	<- totTime + parTime
					matrixRow	<- matrixRow + 1		
					matrixMatrix[matrixRow,] <- c(matrixTokens, totTime)
	  			 	break
				}
			} else {
				index <- inhibIndex[which(inhibIndex[,1] ==rn),2]	
				#see whether and where are inhibitions arcs involved in that transition
				if ((length(index) == 0) || (all(matrixTokens[index] < matrixInhibit[rn,index]))) {
					if (all(matrixTokens >= -matrixOutward[rn,])) {
						#the real transition is completed, adding tokens to postplaces and removing them from preplaces
  						matrixTokens 	<- matrixTokens + matrixDelta[rn,]
						totTime 	<- totTime + parTime
						matrixRow	<- matrixRow + 1		
						matrixMatrix[matrixRow,] <- c(matrixTokens, totTime)
	 	 			 	break
					}
				}
			}

			# the picked transition could be disabled because there is inhibition or because the tokens you have are less than those you need;
			vectorTrans <- vectorTrans[!vectorTrans == rn]	# if that, you repeat the picking until you pick a good one!
			vectorProb  <- vectorProb[-chance]
			if 	(length(vectorTrans) == 0){
				# if you pick all transitions and they are all disabled, you've surely reached a dead state; therefore the whole simulation stops
 				.Internal(cat(list("\t	you reached a dead state!! \t"), stdout(), " ", FALSE, NULL, FALSE))
				matrixRow <- matrixRow + 1
				matrixMatrix[matrixRow,] <- c(matrixTokens, Inf)
				return(matrixMatrix[c(1:matrixRow),])
  			}
		}
		
	}
	return(matrixMatrix[c(1:matrixRow),])
}


####################################
### HERE STARTS THE OUTPUT PART
####################################

tableGlobal		<- matrix(ncol= kplaceN+1, nrow= iterNumber)	#Table to summarize the results of all runs and the mean value of the runs (for each place)
colnames(tableGlobal)	<- c(placesNames, "Dead State?")
totalOutput	 	<- matrix(ncol =kplaceN, nrow= stepsNumber+1)
totalOutput[,] 		<- 0
timeAxis 		<- zoo(0, as.numeric(c(0:stepsNumber)))

for (iterCounter in 1:iterNumber) {
	#the entire simulation is repeated "iterNumber" number of times
	cat ("\n", iterCounter, "iterations of ", iterNumber)
	simulcoreOutput	<- Simulcore()

	timeSeries			<- zoo(simulcoreOutput[,c(1:kplaceN)], simulcoreOutput[,(kplaceN+1)])
	mergedSeries 			<- merge(timeSeries,timeAxis)
	mergedSeries[,c(1:kplaceN)] 	<- na.approx(mergedSeries[,c(1:kplaceN)], rule=2)
	timeIndex 			<- which (index(mergedSeries) %in% index(timeAxis))
	simulcoreOutput2 		<- as.matrix(mergedSeries[timeIndex,c(1:kplaceN)])
	totalOutput			<- totalOutput + simulcoreOutput2

	if (simulcoreOutput[nrow(simulcoreOutput),kplaceN+1] == Inf)
		tableGlobal[iterCounter,] <- c(round(simulcoreOutput2[stepsNumber + 1,],3), "YES")
	else	tableGlobal[iterCounter,] <- c(round(simulcoreOutput2[stepsNumber + 1,],3), "NO")
}

if (iterNumber > 1) {
	totalOutput		<- totalOutput/iterCounter
	tableMean		<- totalOutput[stepsNumber +1,]
	tableGlobal 		<- rbind(tableGlobal, c(round(tableMean,3), ""))
	rownames(tableGlobal)	<- c(1:iterNumber, "Mean")
}

write.table (tableGlobal, outputFile, quote = FALSE, sep = "\t")	#print the summary table and the mean value of the selected place
if (choicePlaceName != "") {
	cat  ("\n therefore, the (mean) value of ", choicePlaceName, " is ", tableGlobal[nrow(tableGlobal), choicePlaceNumber], "for each iteration")
	plot ((0:stepsNumber), totalOutput[,choicePlaceNumber], type="l")
} else
	matplot ((0:stepsNumber), totalOutput, type="l")
