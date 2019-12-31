#import subprocess
#subprocess.call ("Rscript leg-peakfind_v0.01.R", shell=True)

import csv
import numpy as np
import math
import matplotlib.pyplot as plt
from scipy.interpolate import interp1d
from scipy.optimize import bisect

  
def main(filenumber):
    values=[]
    elastic=[]
    plastic=[]
    with open('results/sa20100_' + filenumber + '-es00-ms00-mn01_indt01_result.csv') as csvfile:
        csvfile.next()
        csvfile.next()
        values=[]
        readCSV = csv.reader(csvfile, delimiter=',')
        for row in readCSV:
            
            values.append(row)
    
    values = np.array(values)
    values= values.astype(np.float)
    for n in  range (0,len(values)):
        if values[n,3] <1.0:
            values[n,0]= 0.0
        if values[n,0] <=0.0:
            values[n,0] =0
            #print n
            elastic.append(values[n,:])
        else:
            plastic.append(values[n,:])

    #Elastic Region
            
    elastic = np.array(elastic)
    plastic=np.array(plastic)
    Ri= 200e-6
            
    Reff=Ri
    
    E_list=[]
    p_list=[]
    he_list=[]
    for n in range (0,len(elastic)):
        
        p= elastic [n,3]
        he=elastic [n,2]
        he=he/1e6
        he=math.pow(he,1.5)
        p_list.append(p)
        he_list.append(he)
        
        
    slope_e, intercept = np.polyfit(he_list,p_list, 1)    
    #plt.plot(he_list,p_list)  
    
    Eff=(3*slope_e/(4* math.sqrt(Reff))) 
    
    vi=0.22
    Ei=1050e9
    vs= 0.33
    
    m1=1/Eff
    m2=(1-math.pow(vi,2))/Ei
    Es= (1- math.pow(vs,2))/m1-m2
    Es=Es/1e7
     
    
    #plastic region
    
    ht_list=[]
    pp_list=[]
    for n in range (0,len(plastic)):
        
        p= plastic [n,3]
        ht=plastic [n,2]
        
        ht=ht/1e6
        
        p=math.pow(p,0.6666)
        pp_list.append(p)
        ht_list.append(ht)
        
    #plt.plot(pp_list,ht_list) 
    
    slope_p, intercept_p = np.polyfit(pp_list,ht_list, 1)
    
    Rff=math.pow((1/(math.pow(slope_p,0.6666) *0.75*Eff)),2)
    
    
    strain_list_p=[]
    stress_list_p=[]
    m_factor=700
    for n in range (0,len(plastic)):
        p= plastic [n,3]
         
        ht=plastic [n,2]
        ht=ht/1e6
        hp=plastic[n,0]
        hp=hp/1e6 
        
        a=math.sqrt(Rff*(ht-hp))
        #print hp,a
        stress=((p/((math.pi)*math.pow(a,2)))/1e14)+m_factor
        strain=(8*ht)/(3*math.pi*a)/1e5
        stress_list_p.append(stress)
        strain_list_p.append(strain)
      
    #elasti region
    e_factor=700
    strain_list_e=[]
    stress_list_e=[]
    for n in range (0,len(elastic)):
        
        p= elastic [n,3]
         
        ht=elastic [n,2]
        ht=ht/1e6
        hp=elastic[n,0]
        hp=hp/1e6 
    #    a=math.sqrt(Rff*(ht-hp))
        a=1
        stress=((p/((math.pi)*math.pow(a,2)))/1e-3)+e_factor
        strain=((4*ht)/(3*math.pi*a)/1e-3)*4
        stress_list_e.append(stress)
        strain_list_e.append(strain) 
        
    
    if stress_list_e[-1]> stress_list_p[0]:
        adj_val=stress_list_e[-1]-stress_list_p[0]+50
        for n in range(0,len(stress_list_p)):
            stress_list_p[n]=stress_list_p[n]+adj_val
    
    Strain_list =  strain_list_e +  strain_list_p
    Strain_list[:] = [x / 10 for x in Strain_list]
    Stress_list = stress_list_e + stress_list_p
    final_list= zip(Strain_list,Stress_list)
    final_list=np.array(final_list)

    slope, intercept_p = np.polyfit(strain_list_e,stress_list_e, 1)

#plt.plot(Strain_list,Stress_list, color='green', marker='o')
#
#plt.plot(strain_list_e,stress_list_e, color='green', marker='o')

    strain = final_list[:,0]
    stress = final_list[:,1]


    E = slope *10
    max_stress= max(Stress_list)
    min_sress=min(Stress_list)
    y = [min_sress, max_stress]
    x = 0.002, ((y[1] - y[0])/E+0.002)
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
    #plt.plot(x, y(x))
    plt.scatter(x1, Yield_strength)
    plt.savefig('results2/sa20100_' + filenumber + '-es00-ms00-mn01_indt01_result.png')
    return Yield_strength.item(0), slope, stress(strain)[-1],strain_at_yiled,


for i in range(7, 8):
    try:
        filenumber = "{:0>2d}".format(i) 
        result = main(filenumber)
#        print filenumber + "," + str(result[0]) + "," + str(result[1]) + "\n"
        with open("results2/results.txt", "a") as myfile:
            myfile.write(filenumber + ", " + str(result[0]) + ", " + str(result[1])+ ", "  + str(result[2]) + "\n")
        
    except Exception as e: 
        print filenumber
        continue
    
    #new_stress_list = []
    #
    #for strain in Strain_list:
    #    new_stress_list.append(slope * (strain - 0.02))
    #    
    #plt.plot(Strain_list,new_stress_list, color='red')   
    #    hi=(3* (1-math.pow(vi,2))*p)/(4*Ei*a)
    #    hs=ht-hi
        
    
    
       
    #    Eff= 3 * p /(4 * math.sqrt(Reff * math.pow(he,3)))
    #    Eff_list.append(Eff)
    #    
    #    E=1/((1/Eff)-(1/1050e9))
    #    E_list.append(E)
    
    #

     





