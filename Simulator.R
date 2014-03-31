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

#you can change the path of the input/output files by writing it in the variable "path"
path	  = "."
input_file  = paste (path, "/merge_matrices.txt", sep = "")
output_file = paste (path, "/finalmarkings.txt", sep = "")

#It gives the starting markings, the name of the places/transitions and the arc (inward, outward, inhibitory) weights to be used in the script. It uses a json file as input, but you can re-write this part to use other kinds of input 0

if(!("rjson" %in% rownames(installed.packages()))) {install.packages("rjson")}	#install the package 'rjson'; it'll run it only the first time, and only if you haven't used this package yet
library('rjson')												#every script must load the library
json_data	<- fromJSON(paste(readLines(input_file), collapse=""))

MM = MARKINGS  	<- json_data$marking 	#tokens in all the places at time 0 (i.e. the tokens you've written in the network)
TRnames		<- json_data$tnames
PLnames		<- json_data$pnames	#you'll need those variables later to run the simulation

NP 			= length(PLnames)		# Number of places
TT 			= length(TRnames) 	#Numb of Transitions
MS		= 5000
Iter		= 10

NINHIBIT  		<- do.call(rbind, json_data$inhib)	#matrix of the weights of inhibitory arcs, always going FROM places TO transitions
colnames(NINHIBIT)<- PLnames
NIN 	 		<- do.call(rbind, json_data$post)
colnames(NIN)	<- PLnames
NOUT 	 		<- -1*(do.call(rbind, json_data$pre))
colnames(NOUT) 	<- PLnames
DELTA 		= NIN + NOUT
colnames(DELTA)	<- PLnames
	

cat 	("the places in this network are: \n")
print (PLnames)
repeat
	{
	Optim	= readline ("insert the exact name of the place you're interested in, or press ENTER if you don't want to consider a particular place ")
	if (Optim == "")	
		{cat ("\n \t All places will be considered \n")
		break	
	}else if (Optim %in% PLnames)
		{Optname	= Optim
		Optim		= which (colnames(NIN) == Optim)	#What you're going to count at the end of the run
		break
		}
	}

cat 	("\n The default values are: \n Putative Transitions \t = ", MS, "\n Iterations of the simulation = ", Iter)
choice = readline ("\n Do you want to change these settings? press 1 for yes, anything else for no " )
if (choice ==1)
	{
	repeat
		{
		MS	= type.convert(readline ("insert the number of putative transitions: " ), as.is = TRUE)
			 #The number of transition you want to simulate
		Iter = type.convert(readline ("insert the number of iterations: " ), as.is = TRUE)
			 #The number of times you want to run the script, in order to obtain mean values of the results
		if 	((is.numeric(MS)) && (is.numeric(Iter)))	{break
		}else	{cat	("one or more values are not numeric, please retry \n")}
		}
	}


Globtable			= matrix(ncol= NP, nrow= 0)
#Table to summarize the results of all runs and the mean value of the runs (for each place)

Deadstate = rep (FALSE, Iter)
####################################
### HERE ENDS THE DECLARATION PART
####################################

Simulcore = function()
	{     
	for(x in seq(1,MS,1))		#the core of the simulation; each step is repeated for MS times (x=1 is the starting condition)
		{		
		vector1	= 1:TT
		repeat
			{
			rn 		<- vector1[.Internal(sample(length(vector1), 1, replace = FALSE, prob = NULL))]		#pick a random transition
			index	<-.Internal(which(NINHIBIT[rn,]>=1))	#see whether there are inhibitions arcs involved in that transition (and where they are)
 			
			if	(((length(index) == 0) || (all(MM[index] < NINHIBIT[rn,index]))) && (all(MM >= -NOUT[rn,])))
  				{				#the real transition is completed, adding tokens to postplaces and removing them from preplaces
  				MM = MM + DELTA[rn,]
  			 	break
  			 	}					
			else 
				{				# the picked transition could be disabled because there is inhibition or because the tokens you have are less than those you need;
				vector1 = vector1[!vector1 == rn]	# if that, you repeat the picking until you pick a good one!
				if 	(length(vector1) == 0)	
					{			# if you pick all transitions and they are all disabled, you've surely reached a dead state; therefore the whole simulation stops
	 				.Internal(cat(list("you reached a dead state!! \n"), stdout(), " ", FALSE, NULL, FALSE))
					Deadstate[y] = TRUE
		 			return(MM[])
					}
  				}
			}
		}
	return(MM[])
	}

####################################
### HERE STARTS THE MAIN PART
####################################

for (y in 1:Iter)					#the entire simulation is repeated "Iter" number of times
	{
	output	= Simulcore()
	Globtable	= rbind(Globtable, output)
	cat ("\n", y, "iterations of ", Iter)
	}


if (Iter > 1)
	{
	Mean			= colMeans(Globtable)
	Globtable 		= rbind(Globtable, Mean)
	rownames(Globtable)  =  c(1:Iter, "Mean")
	}
if	(Optim != "")
	{cat ("\n therefore, the (mean) value of ", Optname, " is ", Globtable[nrow(Globtable), Optim], "for each iteration")}


Globtable = cbind(Globtable, c(Deadstate,""))
colnames(Globtable) = c(PLnames, "Dead State?")
write.table (Globtable, output_file, quote = FALSE, sep = "\t")	#print the summary table and the mean value of the selected place
cat ("\n\n Success! It has been created a file containing the final values of all the places at the end of each iteration (and the mean values)")

####################################
### HERE ENDS THE MAIN PART
####################################
