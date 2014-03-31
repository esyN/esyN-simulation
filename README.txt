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
This scripts allow you to simulate the behaviour of your Petri Network.

Input format	= json file (merge_matrices.txt) obtained from the website "http://www.esyn.org"
		  To run the script without making any changes, that file must be placed in the same folder of the Simulator
		  
How to run	= open Simulator.R
		  optionally set the input_path and output_path (if you want to use a folder other than that containing the script)
		  run the script
		  when prompted:
		  	- optionally set a place to monitor (by default all places are monitored)
		  	- optionally change the default the number of steps in each iteraction of the simulation, and the number of steps (transitions that fire) in each iteraction
		  A file "finalmarkings.txt" is created containing the final marking for each iteration and the average over all iteration

Licenses	= in the "Licenses" folder you may find:
		  
		1) a copy of the "GNU General Public License"
		2) a copy of the "GNU Lesser General Public License"
