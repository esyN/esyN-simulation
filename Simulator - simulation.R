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


library('rjson')            # rjson package is loaded; it must be installed before running the script
stepsNumber   <- 3000       # Number of steps of each simulation (not the duration of the simulation itself)
iterNumber	  <- 100        # Number of times each simulation is repeated; all the results will be saved in separate files

inputPath	    <- ""         # by default, the input file will be read from the working directory
outputPath	  <- "Results/" # by default, output files will be placed in this folder inside the working directory
inputFileName	<- "merge_matrices.txt"
inputFile	<- paste (inputPath, inputFileName, sep = "")
inputData	<- fromJSON(paste(readLines(inputFile), collapse=""))

matrixTokens 	<- inputData$marking 			# tokens in all the places at time 0 (i.e. the tokens you've written in the network)
transitNames	<- inputData$tnames			  # Name of transitions
placesNames	<- inputData$pnames			    # Name of places
kplaceN 	<- length(placesNames)		   	# Number of places
ktransitN 	<- length(transitNames) 		# Number of Transitions
#vectorPar	<- rep (1, ktransitN)			  # Mass Action parameters
vectorPar 	<- inputData$k				      # Mass Action parameters
matrixInhibit  	<- do.call(rbind, inputData$inhib)	  # matrix of the weights of inhibitory arcs, always going FROM places TO transitions
inhibIndex 	<- which(matrixInhibit>=1, arr.ind=TRUE)  #see where are inhibitions in the matrix
matrixInward 	<- do.call(rbind, inputData$post)       # matrix of edges going from transitions to places
matrixOutward 	<- -1*(do.call(rbind, inputData$pre)) # matrix of edges going from places to transitions
matrixDelta 	<- matrixInward + matrixOutward
colnames(matrixInhibit) <- colnames(matrixInward) <- colnames(matrixOutward) <- colnames(matrixDelta) <- placesNames


####################################
### HERE STARTS THE SIMULATION PART
####################################

dir.create ("Results", showWarnings = FALSE)

Simulcore <- function() {  # the core of the simulation; it is employed later in the script
  # variables initialized
  matrixMatrix 	<- matrix(ncol =kplaceN+1, nrow= stepsNumber)
  matrixMatrix[1,]<- c(matrixTokens,0)
  totTime 	<- 0
  
  # each step is repeated for stepsNumber times (x=1 is the starting condition)
  for (matrixRow in 1:stepsNumber) {
    # the following lines implement the core of the Gillespie algorithm;
    vectorTrans <- 1:ktransitN				                                  # a probability vector is initialized
    tokenSum    <- rowSums(-1*sweep(matrixOutward,2,matrixTokens,`*`))  # the nr of tokens in all the input places are assigned to each transition
    tokenSum[which(tokenSum == 0)] = 1                                  # source transitions would have 0 tokens; here, 1 token is assigned by default   
    vectorProb  <- tokenSum*vectorPar                                   # probability vector completed
    vectorTime  <- 0
    for (x in 1:ktransitN) {                                            
      # random time is assigned to each transition (according an the exponential distribution with rate=probability)
      if (vectorProb[x] <= 0) {vectorTime[x] = Inf}
      else {vectorTime[x] <-rexp(1,vectorProb[x])}
    }
    
    repeat {
      # the shortest transition is chosen and its time recorded
      parTime	<- min(vectorTime)		
      chance	<- which(vectorTime == parTime)
      if (length(chance) >1)
        chance	<- sample(chance,1)
      rn	<- vectorTrans[chance]			
      
      if (length(inhibIndex) == 0) {
        # option 1: there is no inibitory edge in the network
        if (all(matrixTokens >= -matrixOutward[rn,])) {
          # if the transition is enabled, then it fires adding tokens to postplaces and removing them from preplaces
          matrixTokens <- matrixTokens + matrixDelta[rn,]       # matrixTokens are updated according to the transition chosen
          totTime      <- totTime + parTime                     # total time is updated by adding the duration of the firing transition
          matrixMatrix[matrixRow,] <- c(matrixTokens, totTime)  # matrixTokens and the total elapsed time are recorded in the general matrix
          break
        }
      } else {
        # option 2: there are some inibitory edges in the network
        indexIndex <- inhibIndex[which(inhibIndex[,1] ==rn),2]	#see whether and where are inhibitions arcs involved in that transition
        if ((length(indexIndex) == 0) || (all(matrixTokens[indexIndex] < matrixInhibit[rn,indexIndex]))) {
          if (all(matrixTokens >= -matrixOutward[rn,])) {
            # if the transition is enabled, then it fires , adding tokens to postplaces and removing them from preplaces
            matrixTokens <- matrixTokens + matrixDelta[rn,]      # matrixTokens are updated according to the transition chosen
            totTime	     <- totTime + parTime                    # total time is updated by adding the duration of the firing transition
            matrixMatrix[matrixRow,] <- c(matrixTokens, totTime) # matrixTokens and the total elapsed time are recorded in the general matrix
            break
          }
        }
      }
      
      # if you have reached this point that means that the picked transition was disabled (inhibition or lack of the required number of tokens in the input places);
      vectorTrans <- vectorTrans[!vectorTrans == rn]	# if that, you repeat the picking until you pick a good one!
      vectorTime  <- vectorTime[-chance]
      if 	(length(vectorTrans) == 0){
        # if you have picked all transitions and they were all disabled, you've surely reached a dead state; therefore the whole simulation stops
        .Internal(cat(list("\t	dead state!!"), stdout(), " ", FALSE, NULL, FALSE))
        matrixMatrix[matrixRow,] <- c(matrixTokens, -totTime)  # the last computed matrixTokens is recorded in the matrix together with a negative time (flagging dead states!)
        return(matrixMatrix[c(1:matrixRow),])                  
      }
    }
    
  }
  return(matrixMatrix[c(1:matrixRow),])
}

for (iterCounter in 1:iterNumber) {
  # the entire simulation is repeated "iterNumber" number of times
  tempOutput	<- Simulcore()
  outputFile3  <- paste (outputPath, "simulation", iterCounter,".txt", sep = "")
  # each simulation produces one file containing the general matrix
  if (is.matrix(tempOutput)) {
    write.table  (tempOutput, outputFile3, quote = FALSE, sep = "\t", row.names = FALSE, col.names = c(placesNames,"Time"))
  } else {write.table  (t(tempOutput), outputFile3, quote = FALSE, sep = "\t", row.names = FALSE, col.names = c(placesNames,"Time"))}
  cat ("\n", iterCounter, "iterations completed of ", iterNumber)
}