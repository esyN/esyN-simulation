###   Stochastic Petri Nets!
	#This is the main script, which allows you to run stochastic simulations of your network.

source ("Variables.R")
	#It calls another script which gives the starting markings, the name of the places/transitions and the arc (inward, outward, inhibitory) weights to be used as inputs of this script. At the moment it's automatically called the script which reads from json files, but you can use different Variables.R scripts if you want to use different kind of input data (e.g. txt tables from python scripts) - you just need to rename it "Variables.R"!  

MS 	= as.numeric(readline ("insert the number of putative transitions: " ))
	#The number of transition you want to simulate
Iter	= as.numeric(readline ("insert the number of iterations: " ))
	#The number of times you want to run the script, in order to obtain mean values of the results
Cells	= which (colnames(NIN) == "Cells_counter")
	#What you're going to count at the end of the run (you can change it if you want to count something else)

Ncells 		= 0
Growthtable	<- matrix(ncol=2, nrow=Iter)
colnames(Growthtable)		<- c("N.of cells","Growth rate")
	#Table to summarize the results of all runs

#ptm <- proc.time()
	#time counter, disabled; only useful if you're concerned of the speed of this script (e.g. you want to simulate a huge network)

for (y in 1:Iter)
	#the entire simulation is repeated "Iter" number of times
{
  source ("Simulcore.R")
  results= Simulcore(NIN,NOUT)
  Ncells = results[Cells]
	#the real simulation is carried over in another script, whose output is a vector containing the final markings for each place; you select the place you want.
	#there are a few different scripts, each using its own logic; the (seemingly) fastest one is automatically called, but you can use a different one if you want to test it on your network (using different logic, it could be quicker in your case!)	
	
# Growth = Ncells/MS
  Growth = Ncells^(1/MS)
  	#you can actually measure a linear growth rate (currently disabled) or an exponential one, whathever you prefer
  Growthtable[y,] = c(Ncells,Growth)
	#changes the summary table adding the values of each run
  cat (y, "iterations of ", Iter, "\n")
}

print (Growthtable)
cat("\n therefore,the overall mean growth rate is ", mean(Growthtable[,2]), " cells at each iteration \n")
	#print the summary table and the mean growth rate

#ptm2 <- proc.time() - ptm
#cat (ptm2)
	#time counter, disabled; print the elapsed time of the core of the script