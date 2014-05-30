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


library('rjson')
stepsNumber	<- 3000       # Number of steps of each simulation (not the duration of the simulation itself)
iterNumber	<- 100        # Number of times each simulation is repeated; all the results will be saved in separate files

inputPath	    <- ""
outputPath	  <- "Results/"
inputFileName	<- "merge_matrices.txt"
inputFile	<- paste (inputPath, inputFileName, sep = "")
inputData	<- fromJSON(paste(readLines(inputFile), collapse=""))

matrixTokens 	<- inputData$marking 			# tokens in all the places at time 0 (i.e. the tokens you've written in the network)
transitNames	<- inputData$tnames			  # Name of transitions
placesNames	<- inputData$pnames			    # Name of places
kplaceN 	<- length(placesNames)		   	# Number of places
ktransitN 	<- length(transitNames) 		# Number of Transitions
vectorPar   <- inputData$k				      # Mass Action parameters
matrixInhibit  	<- do.call(rbind, inputData$inhib)	# matrix of the weights of inhibitory arcs, always going FROM places TO transitions
inhibIndex 	<- which(matrixInhibit>=1, arr.ind=TRUE)#see where are inhibitions in the matrix
matrixInward 	<- do.call(rbind, inputData$post)
matrixOutward 	<- -1*(do.call(rbind, inputData$pre))
matrixDelta 	<- matrixInward + matrixOutward
colnames(matrixInhibit) <- colnames(matrixInward) <- colnames(matrixOutward) <- colnames(matrixDelta) <- placesNames


####################################
### HERE STARTS THE SIMULATION PART
####################################

dir.create ("Results", showWarnings = FALSE)

Simulcore <- function() {  #the core of the simulation; each step is repeated for stepsNumber times (x=1 is the starting condition)
	matrixMatrix 	<- matrix(ncol =kplaceN+1, nrow= stepsNumber)
	matrixMatrix[1,]<- c(matrixTokens,0)
	totTime 	<- 0

	for (matrixRow in 1:stepsNumber) {
		vectorTrans <- 1:ktransitN				
		tokenSum    <- rowSums(-1*sweep(matrixOutward,2,matrixTokens,`*`))
		tokenSum[which(tokenSum == 0)] = 1
		vectorProb  <- tokenSum*vectorPar
		vectorProb  <- (-1/vectorProb)*log(runif(ktransitN,min=0, max=1))

		repeat {
			parTime	<- min(vectorProb)		
			chance	<- which(vectorProb == parTime)
			if (length(chance) >1)
				chance	<- sample(chance,1)
			rn	<- vectorTrans[chance]			

			if (length(inhibIndex) == 0) {
				if (all(matrixTokens >= -matrixOutward[rn,])) {
					#the real transition is completed, adding tokens to postplaces and removing them from preplaces
  				matrixTokens <- matrixTokens + matrixDelta[rn,]
					totTime      <- totTime + parTime
					matrixMatrix[matrixRow,] <- c(matrixTokens, totTime)
	  			 	break
				}
			} else {
				indexIndex <- inhibIndex[which(inhibIndex[,1] ==rn),2]	#see whether and where are inhibitions arcs involved in that transition
				if ((length(indexIndex) == 0) || (all(matrixTokens[indexIndex] < matrixInhibit[rn,indexIndex]))) {
					if (all(matrixTokens >= -matrixOutward[rn,])) {
						#the real transition is completed, adding tokens to postplaces and removing them from preplaces
  					matrixTokens <- matrixTokens + matrixDelta[rn,]
						totTime	     <- totTime + parTime
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
 				.Internal(cat(list("\t	dead state!!"), stdout(), " ", FALSE, NULL, FALSE))
				matrixMatrix[matrixRow,] <- c(matrixTokens, -totTime)
				return(matrixMatrix[c(1:matrixRow),])
  			}
		}
		
	}
	return(matrixMatrix[c(1:matrixRow),])
}

for (iterCounter in 1:iterNumber) {
  #the entire simulation is repeated "iterNumber" number of times
  tempOutput	<- Simulcore()
  outputFile3  <- paste (outputPath, "simulation", iterCounter,".txt", sep = "")
  if (is.matrix(tempOutput)) {
    write.table  (tempOutput, outputFile3, quote = FALSE, sep = "\t", row.names = FALSE, col.names = c(placesNames,"Time"))
  } else {write.table  (t(tempOutput), outputFile3, quote = FALSE, sep = "\t", row.names = FALSE, col.names = c(placesNames,"Time"))}
  cat ("\n", iterCounter, "iterations completed of ", iterNumber)
}
