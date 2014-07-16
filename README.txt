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



Welcome to the PetriNet Simulator.
These scripts allow you to simulate the behaviour of your Petri Network and to perform a simple analysis of the simulation results

Licenses	= in the "Licenses" folder you may find:
		  
		1) a copy of the "GNU General Public License"
		2) a copy of the "GNU Lesser General Public License"


SIMULATION SCRIPT
		NOTE: it requires the "rjson" package (Alex Couture-Beil, 6/2014), available at "http://cran.r-project.org/web/packages/rjson/index.html"
		This script simulates the behaviour of your Petri Network; it implements Petri Net rules and the Gillespie algorithm.
		The whole simulation is repeated for 100 times (users can change this "iterNumber" in the script).

		Simulation is repeated for 3000 steps (users can change this "stepsNumber" in the script); at each step:
			first, Petri Net rules are employed to identify which transitions of the network are enabled to fire at each simulation step
			second, the Gillespie algorithm is employed to choose which enabled transition actually fires at each simulation step
				1) rates are calculated for each transition; they are function of the k parameter and the number of tokens in the pre-places (of the selected transition)
OLD VERSION			2) a "waiting time" is calculated for each transition combining the correspondent rate and a randomly generated number
					vectorProb  <- (-1/vectorProb)*log(runif(ktransitN,min=0, max=1))
NEW VERSION			2) a "waiting time" is calculated for each transition by generating a random number from an exponential distribution, whose rate is the transition rate
					vectorTime[x] <-rexp(1,vectorProb[x])  
				3) the transition with the shortest waiting time is the transition that actually fires
			then, rules are employed to move tokens among places according to the firing transition; the simulation time is increased by adding the waiting time
		In this script, all the transitions can fire at the beginning of each step; if a disabled transition is chosen, the simulation is performed but the changes are not applied; 
			the simulation is repeated by choosing among the remaining transitions; if there's no transitions left, the system has reached a dead state and the simulation stops

		the input file is a json file (merge_matrices.txt) obtained from the website "http://www.esyn.org"
			that file should be placed in the same folder of the Simulation script
			(if you've placed your input files in another folder, change the input path inside the script before running it)
		the output file is a matrix showing the amount of tokens in each place and the elapsed time at each simulation step
			a matrix is created at each iteration run; 
			all the matrices are placed inside the "Results" folder, which is located in the same folder of the input file

ANALYSIS SCRIPT
		NOTE: it requires the "zoo" package (Zeileis et al., 2/2014), available at "http://cran.r-project.org/web/packages/zoo/index.html"
		it is a script to perform a simple analysis of the simulation results; therefore, it can be run anytime, but not before the simulation script
			for each simulation file in the folder, values are interpolated at specific time points (according to the number of points you want to sample)
				the number of points ("spotsN") can be changed in the script
			outliers (too short or too long simulations) are identified and discarded for the following steps; 
				the threshold (i.e. distance from the mean value) is expressed as numbers of standard deviations
				this value ("sensSd") can be changed in the script
			a mean time series is created by calculating the mean values at each time point of all the simulations.

		the input files are the matrices located in the "Results" folder
			those files should be already located in the same folder of the Analysis script
			(if you've placed your initial json file in another folder, change the input path inside the script before running it)
		output: in the R interface 		- mean values of selected places, and a graph of the abundance of those places (or all the places) at different times
			in the "finalmarkings.txt" file - values at the last time point for each simulation
			in the "timeseries.txt" file 	- mean values of all the simulation at each time point