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
This scripts allow you to simulate the behaviour of your Petri Network and to perform a simple analysis of the simulation results

Licenses	= in the "Licenses" folder you may find:
		  
		1) a copy of the "GNU General Public License"
		2) a copy of the "GNU Lesser General Public License"

Input format	= json file (merge_matrices.txt) obtained from the website "http://www.esyn.org"
		  that file must be placed in the same folder of the Simulator

Simulation script
		simply run the simulator script
		(if you've placed your input files in another folder, change the input path inside the script before running it)
		for each iteration of the simulation, a file is created in the "Results" folder

Analysis script
		you can run it anytime, but you need to have some files in the "Results" folder to work with		
		(if you've placed your input files in another folder, change the input path inside the script before running it)		

		for each simulation file in the folder, values are interpolated at specific time points (according to the number of points you want to sample)
		a time series is created by calculating the mean values at each time point among all the simulation

		output: in the R interface - mean values of selected places, and a graph of the abundance of those places (or all the places) at different times
			in the "finalmarkings.txt" file (values at the last time point for each simulation)
			in the "timeseries.txt" file (mean values of all the simulation at each time point)
