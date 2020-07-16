# -*- coding: utf-8 -*-
"""
Created on Tue Jan 15 12:32:34 2019

@author: Nishan Senanayake
This script generates the  elastic modulues and the  stress-strain curve from tne input CSV file 

"""

import csv
import numpy as np
import math
import matplotlib.pyplot as plt
from scipy.interpolate import interp1d
from scipy.optimize import bisect
from scipy.stats import linregress



EM_values=[]

with open('CSV file') as csvfile:
    readCSV = csv.reader(csvfile, delimiter=',')
    for row in readCSV:
        EM_values.append(row)


vi=0.22
Ei=1050e9
vs= 0.43
Ri= 200e-6
Reff=Ri


p_list=[]
he_list=[]

#Elastic modulus calculation

EM_values=(np.array(EM_values)).astype(np.float)

for n in range (0,len(EM_values)):
    
    p= EM_values [n,1]
    he=EM_values [n,0]
    he=he/1e6
    he=math.pow(he,1.5)
    p_list.append(p)
    he_list.append(he)


plt.plot(he_list,p_list)    
slope= linregress(he_list, p_list)[0]
Eff= (3*slope)/(4 * math.pow(Ri,0.5))

t1=1/Eff
t2=(1-math.pow(vi,2))/(Ei)
Es=(1-math.pow(vs,2))/(t1-t2)
Es=Es/1e9

elasic=[]
plastic=[]
with open('') as csvfile:
    csvfile.next()
    csvfile.next()
    values=[]
    readCSV = csv.reader(csvfile, delimiter=',')
    for row in readCSV:
        
        values.append(row)

values = np.array(values)
values= values.astype(np.float)
for n in  range (0,len(values)):
    if values[n,0] <=0.0:
        values[n,0] =0
    if values[n,3] <0.5:
        elastic.append(values[n,:])
    else:
        plastic.append(values[n,:])

#Elastic Region
        
elastic = np.array(elastic)
plastic=np.array(plastic)


E_list=[]
p_list=[]
he_list=[]
Es=[]

#From the regression
for n in range (0,len(elastic)):
    
    p= elastic [n,3]
    he=elastic [n,2]
    he=he/1e6
    he=math.pow(he,1.5)
    p_list.append(p)
    he_list.append(he)



plt.plot(he_list,p_list)    
slope= linregress(he_list, p_list)[0]
Eff= (3*slope)/(4 * math.pow(Ri,0.5))

t1=1/Eff
t2=(1-math.pow(vi,2))/(Ei)
Es=(1-math.pow(vs,2))/(t1-t2)
Es=Es/1e9


#elastic region 

strain_list_e=[]
stress_list_e=[]

for n in range (0,len(elastic)):
       
    p= elastic [n,3]
    h= elastic [n,2]
    h=h/1e6
    a=math.sqrt(Reff)
    hi=(3*(1-math.pow(vi,2))*p)/(4*Ei*a)
    stress=p/(math.pi*math.pow(a,2))
    hs=h-hi
    strain= (4*hs)/(3*math.pi*a)
    stress_list_e.append(stress/1e1)
    strain_list_e.append(strain)


plt.plot(strain_list_e,stress_list_e) 



#plastic region

hp_list=[]
pp_list=[]
for n in range (0,len(plastic)):
    
    h= plastic [n,2]
    h=h/1e6
    p= plastic [n,3]
    p=math.pow(p,0.666)
    hp_list.append(h)
    pp_list.append(p)

hp_list = np.array(hp_list)
pp_list=np.array(pp_list)



slope_p= linregress(pp_list,hp_list)[0]
hp=Intercept_p= linregress(pp_list,hp_list)[1]   

Rff_s1= (4* Eff*math.pow(slope_p,1.5))/3
Reff_p=1/Rff_s1


strain_list_p=[]
stress_list_p=[]

for n in range (0,len(plastic)):
       
    p= plastic [n,3]
    h= plastic [n,2]
    h=h/1e6
    hp=plastic[n,0]
    hp=hp/1e6 
    a=math.sqrt(Reff_p*(h-hp))
    hi=(3*(1-math.pow(vi,2))*p)/(4*Ei*a)
    stress=p/(math.pi*math.pow(a,2))
    hs=h-hi
    strain= (4*hs)/(3*math.pi*a)
    stress_list_p.append(stress/1e4)
    strain_list_p.append(strain)


plt.plot(strain_list_p,stress_list_p,"o") 


Strain_list =  strain_list_e +  strain_list_p
Stress_list = stress_list_e + stress_list_p


#Generating the plot

plt.plot(Strain_list,Stress_list) 



E, intercept_p = np.polyfit(strain_list_e,stress_list_e, 1)
max_stress= max(Stress_list)
min_sress=min(Stress_list)
y = [min_sress, max_stress]
x = 0.02, ((y[1] - y[0])/E+0.02)
y = interp1d(x, y)
    
stress = interp1d(strain, stress)
    
#set starting points
x1 = max(x[0], strain[0])
x2 = min(x[-1], strain[-1])
max_err = .001
    
#create function
f = lambda x : stress(x) - y(x)
    
    #find x1 where f(x1) = 0
x1 = bisect(f, x1, x2, xtol = .001)
    
Yield_strength = stress(x1)
strain_at_yiled= x1
    
plt.figure(num=None, figsize=(10, 6), dpi=100, facecolor='w', edgecolor='k')
plt.ylabel('Stress (MPa)')
plt.xlabel('Strain (%)')
plt.plot(strain, stress(strain),color='green', marker='o')

slope_p, intercept_p = np.polyfit(pp_list,ht_list, 1)

