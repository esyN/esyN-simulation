Simulcore <- function(NIN,NOUT){
#this is the real simulation script, where the real simulation takes place; it's wrapped as a function which returns a vector containing the tokens of all places at the end of the simulation

MM 		<- matrix(ncol=NP, nrow =MS)
MM[1,]	<- MARKINGS
	#basically, a matrix containing all the values of the tokens per each simulation step
#GCL 	= which (PLnames == "GCL")
#Substr	= which (PLnames == "Substrates")
	#disabled; you need them in order to consider that the external glucose&substrates available for each cell decrease when the number of cell increases
	
for(x in seq(2,MS,1))
 	{
 	#the core of the simulation; each step is repeated for MS times (x=1 is the starting condition)

	vector1	= 1:TT
  	rn <- sample(vector1, 1)		#pick a random transition
  	# cat ("transition is: t",rn,"\n", sep="")
  	index<-which(NINHIBIT[rn,]>=1)	#see whether there are inhibitions arcs involved in that transition (and where they are)
  	#cat ("places of inhibition are: p",index,"\n",sep="")
  	
  	while	(!(((length(index) == 0) || (all(MM[(x-1),index] < NINHIBIT[rn,index]))) && (all(MM[(x-1),] >= -NOUT[rn,]))))
  		# the picked transition could be disabled because there is inhibition or because the tokens you have are less than those you need
  		# if that, you repeat the picking until you pick a good one!
		{
		vector1 = vector1[-rn]
		rn <- sample(vector1, 1)
			# this way, you're not going to pick a transition you've already discharged!
		index<-which(NINHIBIT[rn,]>=1)
		if (length(vector1) == 0)	{break}
  		}
  	
  	if (length(vector1) == 0)
		{
		#if you pick all transitions and they are all disabled, you've surely reached a dead state; therefore the whole simulation stops, no matter how many steps are left to be done, and an absurd value (negative number of cells) is returned as a mark that something has gone wrong!
	 	cat	("you reached a dead state!! \n")
	 	MM[x,] = -1
	 	break
	 	}
	else
		{	
 	 	MM[(x),] = NOUT[rn,] + MM[(x-1),] + NIN[rn,]
 	 		#the real transition is completed, adding tokens to postplaces and removing them from preplaces
   		#cat ("pp ",MM[(x+1),],"\n\n")
  		#MM[x, GCL] 	= round (MM[x, GCL]		*(MM[(x-1), Cells] + 1)/(MM[x, Cells] + 1))
		#MM[x, Substr] 	= round (MM[x, Substr]	*(MM[(x-1), Cells] + 1)/(MM[x, Cells] + 1))
		#those lines are needed to simulate the fact that when the cells increase, the food available for each cell is exponentially reduced 
   	 	}	
  	}
return(MM[x,])
}