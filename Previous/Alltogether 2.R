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
### HERE STARTS THE DECLARATION PART
####################################

#It gives the starting markings, the name of the places/transitions and the arc (inward, outward, inhibitory) weights to be used in the script. It uses a json file as input, but you can re-write this part to use other kinds of input 

if(!("rjson" %in% rownames(installed.packages()))) {install.packages("rjson")}			#install the package 'rjson' if not already installed
library('rjson')										#every script must load the library
json_data	<- fromJSON(paste(readLines("Matrices/merge_matrices.txt"), collapse=""))

	
MARKINGS  	<- json_data$marking 	#tokens in all the places at time 0 (i.e. the tokens you've written in the network)
TRnames		<- json_data$tnames
PLnames		<- json_data$pnames	#you'll need those variables later to run the simulation
NP 			= length(PLnames)	# Number of places
TT 			= length(TRnames) 	#Numb of Transitions

NINHIBIT  		<- do.call(rbind, json_data$inhib)	#matrix of the weights of inhibitory arcs, always going FROM places TO transitions
colnames(NINHIBIT) 	<- PLnames
NIN 	 		<- do.call(rbind, json_data$post)
colnames(NIN)	<- PLnames
NOUT 	 		<- -1*(do.call(rbind, json_data$pre))
colnames(NOUT) 	<- PLnames
DELTA 			= NIN + NOUT
colnames(DELTA)	<- PLnames
	

MS	= 5000
Iter	= 10
Cells 	= which (colnames(NIN) == "Cells_counter")		#What you're going to count at the end of the run (you can change it if you want to count something else)

cat 	("The default values are: \n Putative Transitions \t \t = ", MS, "\n Iterations of the simulation \t = ", Iter)
choice	= readline ("\n Do you want to change these settings? press 1 for yes, anything else for no " )
if (choice ==1)
	{
	MS 	= as.numeric(readline ("\ninsert the number of putative transitions: " ))	#The number of transition you want to simulate
	Iter	= as.numeric(readline ("insert the number of iterations: " ))			#The number of times you want to run the script, in order to obtain mean values of the results	
	}

Ncells 			= 0
Growthtable		<- matrix(ncol=2, nrow=Iter)
colnames(Growthtable)	<- c("N.of cells","Growth rate")	#Table to summarize the results of all runs
	

####################################
### HERE ENDS THE DECLARATION PART
####################################



####################################
### HERE STARTS THE MAIN PART
####################################
ptm <- proc.time()

for (y in 1:Iter)			#the entire simulation is repeated "Iter" number of times
   {
   MM		<- MARKINGS		#basically, a matrix containing all the values of the tokens per each simulation step
   for(x in seq(1,MS,1))		#the core of the simulation; each step is repeated for MS times (x=1 is the starting condition)
	{
	vector1	= 1:TT
  	rn 		<- vector1[.Internal(sample(length(vector1), 1, replace = FALSE, prob = NULL))]		#pick a random transition
  	index	<-.Internal(which(NINHIBIT[rn,]>=1))	#see whether there are inhibitions arcs involved in that transition (and where they are)
  	
  	while	(!(((length(index) == 0) || (all(MM[index] < NINHIBIT[rn,index]))) && (all(MM >= -NOUT[rn,]))))
  		# the picked transition could be disabled because there is inhibition or because the tokens you have are less than those you need
  		# if that, you repeat the picking until you pick a good one!
		{
		vector1 = vector1[!vector1 == rn]
		rn 		<- vector1[.Internal(sample(length(vector1), 1, replace = FALSE, prob = NULL))]		# you're not going to pick a transition you've already discharged!
		index	<-.Internal(which(NINHIBIT[rn,]>=1))
		if (length(vector1) == 0)	{break}
  		}
  	
  	if (length(vector1) == 0)
		{
		#if you pick all transitions and they are all disabled, you've surely reached a dead state; therefore the whole simulation stops, no matter how many steps are left to be done, and an absurd value (negative number of cells) is returned as a mark that something has gone wrong!
	 	.Internal(cat(list("you reached a dead state!! \n"), stdout(), " ", FALSE, NULL, FALSE))
	 	MM[] = -1
	 	break
	 	}
	else
		{MM = MM + DELTA[rn,]}	#the real transition is completed, adding tokens to postplaces and removing them from preplaces
	}  

   Ncells = MM[Cells]	
#  Growth = Ncells/MS
   Growth = Ncells^(1/MS)
  	#you can measure a linear growth rate (currently disabled) or an exponential one, whathever you prefer
   Growthtable[y,] = c(Ncells,Growth)
	#changes the summary table adding the values of each run
   cat ("\n", y, "iterations of ", Iter)
   }

cat ("\n\n")
print (Growthtable)
cat("\n therefore,the overall mean growth rate is ", mean(Growthtable[,2]), " cells at each iteration \n")
	#print the summary table and the mean growth rate

ptm2 <- proc.time() - ptm
cat (ptm2)
####################################
### HERE ENDS THE MAIN PART
####################################