#this is the real simulation script, where the real simulation takes place; it's wrapped as a function which returns a vector containing the tokens of all places at the end of the simulation

Simulcore <- function(NIN,NOUT){
	
#  declaration part -begin
	MM		<- matrix(ncol=NP, nrow =MS)
	woma	= matrix(ncol = NP, nrow = 1)
	woma[1,]= MM[1,] <- MARKINGS
	woma 	= woma[rep(1,TT),]
		#basically, MM is a matrix containing all the values of the tokens per each simulation step
		#woma is a matrix where the initial tokens are repeated in all rows (it has no real meaning, but you'll need it later)

	index2	= which(NINHIBIT !=0, arr.ind = TRUE)	#see whether there are inhibitions arcs in the network and where they are
	inhibit = NINHIBIT[index2] + NOUT[index2]		#it has no real meaning, but you'll need it later
	deltamat	= woma + NOUT						#basically, a matrix which contains the outcome of all possible translations! (that's why we needed woma)
	
	#GCL 	= which (PLnames == "GCL")
	#Substr	= which (PLnames == "Substrates")
		#disabled; you need them in order to consider that the external glucose&substrates available for each cell decrease when the number of cell increases
#  declaration part -end

for(x in seq(2,MS,1) )
	#the core of the simulation; each step is repeated for MS times (x=1 is the starting condition)
	{
	vector1	= 1:TT												#all the possible transitions
	vector2	= unique(which(deltamat <0, arr.ind = TRUE)[,1])	#all the disabled transitions

	if (!(all(deltamat[index2] < inhibit)))
		#it checks whether the inhibition (if present) actually is enabled, and therefore disables the transition, or not (that's why we needed inhibit)
		{
		vector0	= unique(which(!(deltamat[index2] < inhibit)))	#all the actively inhibited transitions
		vector1 	= vector1[!vector1%in%index2[vector0]]		#all the possibile non-inhibited transitions
		}
 	 	
	if 	(length(vector2) >= length(vector1))
		#it checks whether is there any transition enabled or not. If not, you've surely reached a dead state; therefore the whole simulation stops, no matter how many steps are left to be done, and an absurd value (negative number of cells) is returned as a mark that something has gone wrong!
		{
	 	cat	("you reached a dead state!! \n")
	 	MM[x,] = -1
	 	break
	 	}
	else
		{
		vector3 = vector1[!vector1%in%vector2]					#all the enabled non-inhibited transitions
		rn 	= sample(vector3, 1)								#pick a random (enabled non-inhibited) transition
		MM[x,] = deltamat[rn,] + NIN[rn,]						#the real transition is completed, adding tokens to postplaces and removing them from preplaces
		
		#MM[x, GCL] 	= round (MM[x, GCL]		*(MM[(x-1), Cells] + 1)/(MM[x, Cells] + 1))
		#MM[x, Substr]	= round (MM[x, Substr]	*(MM[(x-1), Cells] + 1)/(MM[x, Cells] + 1))
			#those lines are needed to simulate the fact that when the cells increase, the food available for each cell is exponentially reduced 
    	
    	deltamat2 		= matrix(ncol = NP, nrow = 1)
		deltamat2[1,]	= MM[x,] - MM[(x-1),]
		deltamat2		= deltamat2[rep(1,TT),]
		deltamat		= deltamat + deltamat2
			#tokens have been moved, therefore there might be new disabled and enabled transitions. The script calculates the difference between previous and present token configuration, and then add it to the matrix of all possible outcomes (there's no need to change the original woma matrix because NOUT is kept constant)
	 	}	
  	}
 return(MM[x,])  	
}