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


###
# parse snoopy matlab export
###

import csv
import	os
import fnmatch
import shutil

for root, dirs, files in os.walk('.'):
    for filename in fnmatch.filter(files,'*.m'):
	print (os.path.join(root, filename))

bah = raw_input ("insert the name of the MatLab file: ")
f = open(bah,'r')

tm_pre = []
tm_post = []
inhib = []
place_names = []
transition_names = []
pre_flag = False
post_flag = False
inhib_flag = False

for line in f:
    if 'pn.P ' in line: #need to include the space to exclude pn.Prearcs etc
        #process place names
        place_names = line.split("'")[1::2] #get only the odd elements after the split
    elif 'pn.T ' in line:
        #process transition names
        transition_names = line.split("'")[1::2]
    elif 'pn.m0' in line:
        #process marking matrix
        #marking = line.split("[")[1].split(']')[0] #this is a string with the amounts separated by commas
        marking = line.split("= ")[1][:-1][1:-2].split(',')

    elif 'pn.T' in line:
        #process transition names
        tm_names = line.split("'")[1::2]
    elif 'pn.PreArcs' in line:
        #process prearcs - will be found on the next few lines
        pre_flag = True
        start = False #used so that the next line will be processed but not this one
    elif 'pn.PostArcs' in line:
        #process postarcs
        post_flag = True
        start = False
    elif 'pn.InhibitorArcs' in line:
    	#inhibitor arcs will be found on the next few lines
    	inhib_flag = True
    	start = False
        

    #processing of prearcs if flag set
    if len(tm_pre) == len(place_names): #there are as many rows as there are places
        pre_flag = False #we have processed all rows

    if pre_flag == True:
        if start == True:
            tm_pre.append(line.split(';')[0].split(','))
 
        else:
            start = True #the next time start will be true and the line will be used

    if len(tm_post) == len(place_names): #there are as many rows as there are places
        post_flag = False #we have processed all rows
    #processing of postarcs if flag set
    if post_flag == True:
        if start == True:
            tm_post.append(line.split(';')[0].split(','))
 
        else:
            start = True #the next time start will be true and the line will be used
            
    #processing of inhibitor arcs if flag set
    if len(inhib) == len(place_names): #there are as many rows as there are places
	inhib_flag = False #we have processed all rows

    #processing of inhibitors if flag set
    if inhib_flag == True:
        if start == True:
            inhib.append(line.split(';')[0].split(','))
 
        else:
            start = True #the next time start will be true and the line will be used
	
f.close()


#transpose the pre and post transition matrices to the format we use
tm_pre_t = zip(*tm_pre)
tm_post_t = zip(*tm_post)
inhib_t = zip(*inhib)

            
#print place_names
#print marking
#print tm_names
#print tm_pre
#print tm_pre_t
#print tm_post
#print tm_post_t
#print inhib_t

#format ready for import

if not os.path.exists('Matrices'):
    os.makedirs('Matrices')


#place names
f = open('Matrices/place_names.txt','wb')
wr = csv.writer(f, dialect='excel',delimiter='\t')
wr.writerow(place_names)
f.close()

#transition names
f = open('Matrices/transition_names.txt','wb')
wr = csv.writer(f, dialect='excel',delimiter='\t')
wr.writerow(transition_names)
f.close()

#preArcs
f = open('Matrices/prearcs.txt','wb')
wr = csv.writer(f, dialect='excel',delimiter='\t')
wr.writerow(place_names)
wr.writerows(tm_pre_t)
f.close()

#postArcs
f = open('Matrices/postarcs.txt','wb')
wr = csv.writer(f, dialect='excel',delimiter='\t')
wr.writerow(place_names)
wr.writerows(tm_post_t)
f.close()

#namedmarking
f = open('Matrices/namedmarking.txt','wb')
wr = csv.writer(f, dialect='excel',delimiter='\t')
wr.writerow(place_names)
wr.writerow(marking)
f.close()

#marking
f = open('Matrices/marking.txt','wb')
wr = csv.writer(f, dialect='excel',delimiter='\t')
wr.writerow(marking)
f.close()

#inhibitor arcs
f = open('Matrices/inhibitorarcs.txt','wb')
wr = csv.writer(f, dialect='excel',delimiter='\t')
wr.writerow(place_names)
wr.writerows(inhib_t)
f.close()