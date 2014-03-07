###Copyright 2014 Daniel Bean
###  
###This file is part of “Petri Net Simulator”.
###
###“Petri Net Simulator” is free software: you can redistribute it and/or modify
###it under the terms of the GNU Lesser General Public License as published by
###the Free Software Foundation, either version 3 of the License, or
###(at your option) any later version.
###
###“Petri Net Simulator” is distributed in the hope that it will be useful,
###but WITHOUT ANY WARRANTY; without even the implied warranty of
###MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
###GNU Lesser General Public License for more details.
###
###You should have received a copy of the GNU Lesser General Public License
###along with “Petri Net Simulator”.  If not, see <http://www.gnu.org/licenses/>.


{
#this script uses the json file (e.g. output from cytoscape or downloaded from the website) in order to obtain the matrices and vectors we need
#install.packages('rjson')	#install the package 'rjson', disabled; you need to enable it only the first time, and only if you haven't used this package already 
library('rjson')			#every script must load the library

json_data <- fromJSON(paste(readLines("../Matrices/generated_matrices.txt"), collapse="")) #you can give a file path where I have "generated_matrices.txt"
	
MARKINGS  <- json_data$marking 	#tokens in all the places at time 0 (i.e. the tokens you've written in the network)
TRnames	  <- json_data$tnames
PLnames	  <- json_data$pnames	#you'll need those variables later to run the simulation

NINHIBIT  			<- do.call(rbind, json_data$inhib)		#matrix of the weights of inhibitory arcs, always going FROM places TO transitions
colnames(NINHIBIT) 	<- PLnames

#all this first part isn't useful at the moment, therefore it's disabled; it'd be useful if you had arcs weights different from those inside the network	
#myswitch2 = readline ("\n Do you want to use new optimized weights if available? press 1 for yes " )
#if	((myswitch2 == 1) && (file.exists("../Matrices/postarcsBEST.txt"))) 
#	{
#	NIN 	  <- as.matrix(read.delim("../Matrices/postarcsBEST.txt"))		#matrix of the weights of arcs going FROM transitions TO places
#	colnames(NIN)	<- PLnames
#	NOUT 	  <- -1*(as.matrix(read.delim("../Matrices/prearcsBEST.txt")))	#matrix of the weights of arcs going FROM places TO transitions
	#we want negative values but they are positive in the original file, therefore we added "-1"; if they are already negative in your tab, please delete that "-1"
#	colnames(NOUT)	<- PLnames				
#	}
#else
#	{
#	cat ("\n no optimized files chosen or available; standard weights will be used \n")
	NIN 	 		<- do.call(rbind, json_data$post)
	colnames(NIN)	<- PLnames
	NOUT 	 		<- -1*(do.call(rbind, json_data$pre))
	colnames(NOUT) 	<- PLnames	
#	}

NP 	= ncol(NIN) # Number of places
TT 	= nrow(NIN) #Numb of Transitions
}