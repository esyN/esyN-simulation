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


Simulcore <- function(NIN,NOUT){

	NOUT = -1*NOUT
	check.transition <- function(NOUT,MM)
	{all(MM>= NOUT)}

	diff <- NIN - NOUT #the overall change caused by each transition. Calculate here to avoid working out every time in the loop
	to.check<-NULL #what transitions should be updated after a given transition has fired

	#GCL 	= which (PLnames == "GCL")
	#Substr	= which (PLnames == "Substrates")

	for(i in 1:TT)
		{
		#for each transition what places are affected
  		changed<-which(diff[i,] != 0)
  		#for each of the affected places, what transitions are they involved in
  		affected<-NULL
  		for(j in 1:length(changed))
  			{
    		affected<-c(affected, which(NOUT[,changed[j]] != 0))
    		}
		affected<-unique(affected)
		to.check[[i]] <- affected
		}
	
	
	MM			<- matrix(ncol=NP, nrow =MS)
	MM[1,] 		<- MARKINGS
	
	index2		= which(NINHIBIT !=0, arr.ind = TRUE) #location of any inhibitor arcs
  	is.enabled<-apply(NOUT,1,check.transition,MM=MM[1,])
  	#set any inhibited transitions to FALSE
  	#if any connected inhibitor arc is enabled, the transition cannot fire. The row to set to FALSE is column 1 for the corresponding row of index2
  	is.enabled[index2[MM[1,index2[,2]]>=NINHIBIT[index2],1]]<-FALSE 
 
 
	for(x in seq(2,MS,1) )
  		{	
   		#the first time through the loop the enabled transitions have already been calculated
    	#now pick a transition at random from our list of enabled transitions
    	n<-c(1:TT)[is.enabled] #row index of enabled transitions in the full matrix
  		rn 	= sample(1:length(n), 1)  	    	 
  		MM[x,] = MM[x-1,] + diff[n[rn],]
  		
  		#MM[x, GCL] 	= round (MM[x, GCL]		*(MM[(x-1), Cells] + 1)/(MM[x, Cells] + 1))
		#MM[x, Substr]	= round (MM[x, Substr]	*(MM[(x-1), Cells] + 1)/(MM[x, Cells] + 1))
		#those lines are needed to simulate the fact that when the cells increase, the food available for each cell is exponentially reduced 

    
    	#update is.enabled
    	update<-to.check[[n[rn]]]
    	is.enabled[update]<-apply(NOUT[update,],1,check.transition,MM=MM[x,])
    	#set any inhibited transitions to FALSE
    	#if any connected inhibitor arc is enabled, the transition cannot fire. The row to set to FALSE is column 1 for the corresponding row of index2
    	is.enabled[index2[MM[x,index2[,2]]>=NINHIBIT[index2],1]]<-FALSE 
    
    	if	(all(is.enabled==FALSE))
			{
     	 	cat('dead state \n')
     	 	MM[x,] = -1
      		break #handling of dead states in the results below needs to be decided. At the moment it's equivalent to extrapolating
    		}	
		}

return(MM[x,]) 
}

