###Copyright 2014 Lorenzo Ficorella
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
#this script uses the tables produced by the python script and convert them into matrices and vectors; more in detail:
#Snoopy -- export network as --> 	MatLab file --converted by a Python script in --> 	txt tables -- read and converted by this script into -->	matrices

#all this first part isn't useful at the moment, therefore it's disabled; it'd be useful if you had arcs weights different from those inside the network
#myswitch2 = readline ("\n Do you want to use new optimized weights if available? press 1 for yes " )
#if	(myswitch2 == 1)
#	{
#	if (file.exists("../Matrices/postarcsBEST.txt"))
#		#the new matrices you would use should be named this way, and should be placed among the other matrices
#		{
#		NIN 	  <- as.matrix(read.delim("../Matrices/postarcsBEST.txt"))
#		NOUT 	  <- -1*(as.matrix(read.delim("../Matrices/prearcsBEST.txt")))				
#		}
#	else
#		{
#		cat ("\n no optimized files available; standard weights will be used \n")
#		#it's only to be sure nobody tries to call for a file which doesn't exist!
#		NIN 	  <- as.matrix(read.delim("../Matrices/postarcs.txt"))
#		NOUT 	  <- -1*(as.matrix(read.delim("../Matrices/prearcs.txt")))
#		}	
#	}	
#else
#	{
	NIN 	  <- as.matrix(read.delim("../Matrices/postarcs.txt"))
		#matrix of the weights of arcs going FROM transitions TO places
	NOUT 	  <- -1*(as.matrix(read.delim("../Matrices/prearcs.txt")))
		#matrix of the weights of arcs going FROM places TO transitions
		#we want negative values but they are positive in the original file, therefore we added "-1"; if they are already negative in your tab, please delete that "-1"
#	}

NINHIBIT  <- as.matrix(read.delim("../Matrices/inhibitorarcs.txt"))
	#matrix of the weights of inhibitory arcs, always going FROM places TO transitions
MARKINGS  <- as.matrix(read.table("../Matrices/marking.txt"))
	#tokens in all the places at time 0 (i.e. the tokens you've written in the network)
TRnames	  <- read.table("../Matrices/transition_names.txt")
PLnames	  <- read.table("../Matrices/place_names.txt")
NP 	= ncol(NIN) # Number of places
TT 	= nrow(NIN) #Numb of Transitions
	#you'll need those variables later to run the simulation
}