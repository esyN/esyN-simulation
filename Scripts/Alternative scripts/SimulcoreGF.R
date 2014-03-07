#this is the real simulation script, where the real simulation takes place; it's wrapped as a function which returns a vector containing the tokens of all places at the end of the simulation

Simulcore <- function(NIN,NOUT){

MSF		= 1000000 		#the maximum number of trial the script would carry on before interrupting itself
MM 		<- matrix(ncol=NP, nrow =MSF)
MM[1,] 	<- MARKINGS		#basically, a matrix containing all the values of the tokens per each simulation step
cavea 	= 0
	
#GCL 	= which (PLnames == "GCL")
#Substr	= which (PLnames == "Substrates")
	#disabled; you need them in order to consider that the external glucose&substrates available for each cell decrease when the number of cell increases
realt = 0
xx 	  = 0

for(x in seq(2,MSF,1))
  #the core of the simulation; each step is repeated for MS times (x=1 is the starting condition)
  {
  if (realt >= MS)		
  	{break} 		

  else
  	{
	rn <- sample(1:TT, 1)				#pick a random transition
	#cat ("transition is: t",rn,"\n", sep="")
 	index<-which(NINHIBIT[rn,]>=1)		#see whether there are inhibitions arcs involved in that transition (and where they are)
  	
  	if (((length(index) == 0) || (all(MM[(x-1),index] < NINHIBIT[rn,index]))) && (all(MM[(x-1),] >= -NOUT[rn,])))	
  	 	# the picked transition could be disabled because there is inhibition or because the tokens you have are less than those you need
  		# if not, you can actually do the transition and update the counter of real transitions
  		{
    	MM[(x),] = NOUT[rn,] + MM[(x-1),] + NIN[rn,]
    	realt = realt+1
    	cavea 	= 0
    	#MM[x, GCL] 	= round (MM[x, GCL]		*(MM[(x-1), Cells] + 1)/(MM[x, Cells] + 1))
		#MM[x, Substr]	= round (MM[x, Substr]	*(MM[(x-1), Cells] + 1)/(MM[x, Cells] + 1))
		#those lines are needed to simulate the fact that when the cells increase, the food available for each cell is exponentially reduced 
  		}
	else
		# if yes, you do a fake transition: the pattern of tokens doesn't change, and the counter of real transition doesn't update itself
		{
		MM[x,] = MM[(x-1),]
		cavea= cavea+1		
		}
	
	
	if (cavea == 10*TT)
		# you count how many fake transition have you done since the last real one, and if they are too many then you have probably reached a dead state! therefore the whole simulation stops, no matter how many steps are left to be done, and an absurd value (negative number of cells) is returned as a mark that something has gone wrong!
		{
	 	cat	("you probably reached a dead state!! \n")
	 	MM[x,] = -1
	 	break
	 	}
  	} 
  xx = x  
  }
return(MM[xx,])
}
