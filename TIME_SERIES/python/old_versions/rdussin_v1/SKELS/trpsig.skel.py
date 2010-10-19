import matplotlib.pylab as plt
from Numeric import *

###############################################################################################
### MY FILE HIERARCHY
###############################################################################################

stockdir='/stock0/stock/dufour/'

###############################################################################################
### DRAKKAR FILE HIERARCHY
###############################################################################################

config='CCOONNFFIIGG'
datadir=stockdir + 'CCOONNFFIIGG/CCOONNFFIIGG-CCAASSEE-MONITOR/'
plotdir=stockdir + 'CCOONNFFIIGG/PLOTS/CCOONNFFIIGG-CCAASSEE/TIME_SERIES/'

###############################################################################################
### GLOBAL READING OF THE MTL FILES
###############################################################################################

f=open(datadir + 'CCOONNFFIIGG-CCAASSEE_TRPSIG.mtl','r')
lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
f.close()

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

datalist1=['unused1','sigma'] # first line
datalist2=['year','trpsig1','trpsig2'] # following lines
nbdata=len(datalist2) - 1
computedlist=['transport1','transport2']
fig_size =  [15.,15.]
nbfig=2
titleplot=['Faroe Bank Channel','Denmark Strait']

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "trpsig.py : use default values (for global)"
elif config.find('PERIANT') == 0 :
        titleplot=['Drake Passage','South Australia']
        print "trpsig.py : use custom values for PERIANT configs"
else :
	print "trpsig.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

for k in range(len(datalist1)) :
	vars()[datalist1[k]]=[]

for k in range(len(datalist2)) :
	vars()[datalist2[k]]=[]

for k in range(len(computedlist)) :
	vars()[computedlist[k]]=[]

###############################################################################################
### DATA MANIPULATION
###############################################################################################

for chaine in lignes[0:1] :
	element=chaine.split()
	vars()[datalist1[0]].append(float(element[0]))
	for k in range(1,len(element)) :
		vars()[datalist1[1]].append(-1*float(element[k]))

for n in range(1,nbdata+1) :
	for chaine in lignes[n::nbdata] :
		element=chaine.split()
		if n == 1 :
			vars()[datalist2[0]].append(float(element[0]))
		for k in range(1,len(element)) :
			vars()[datalist2[n]].append(float(element[k]))

for n in range(1,nbdata+1) :
	vars()[datalist2[n]] = reshape( vars()[datalist2[n]] , (len(vars()[datalist2[0]]),-1) )


for n in range(len(computedlist)) :
	for y in range(len(vars()[datalist2[0]])) :
		vars()[computedlist[n]].append(sum(vars()[datalist2[n+1]][y,:]))

###############################################################################################
### PLOTS part 1 : cosmetics (depends on the config)
###############################################################################################

params = {'axes.labelsize': 12,
          'text.fontsize': 12,
          'legend.fontsize': 12,
          'xtick.labelsize': 12,
          'ytick.labelsize': 12,
          'figure.figsize': fig_size}
plt.rcParams.update(params)

ncontour=25

###############################################################################################
### PLOTS part 2 : general script for plotting
###############################################################################################

plt.subplot(nbfig,nbdata,1)
plt.contourf(vars()[datalist2[0]],vars()[datalist1[1]],transpose(abs(vars()[datalist2[1]])),ncontour)
plt.grid(True)
plt.axis([min(vars()[datalist2[0]]), max(vars()[datalist2[0]]), 
min(vars()[datalist1[1]]),max(vars()[datalist1[1]])])
plt.ylabel('Sigma classes',fontsize='x-large')
plt.title(titleplot[0],fontsize='large')
plt.colorbar()

plt.subplot(nbfig,nbdata,2)
plt.contourf(vars()[datalist2[0]],vars()[datalist1[1]],transpose(abs(vars()[datalist2[2]])),ncontour)
plt.grid(True)
plt.axis([min(vars()[datalist2[0]]), max(vars()[datalist2[0]]), 
min(vars()[datalist1[1]]),max(vars()[datalist1[1]])])
plt.ylabel('Sigma classes',fontsize='x-large')
plt.title(titleplot[1],fontsize='large')
plt.colorbar()

plt.subplot(nbfig,nbdata,3)
plt.plot(vars()[datalist2[0]], vars()[computedlist[0]], 'b')
plt.grid(True)
plt.axis([min(vars()[datalist2[0]]), max(vars()[datalist2[0]]),
min(vars()[computedlist[0]]),max(vars()[computedlist[0]])])
plt.ylabel('Transport',fontsize='x-large')

plt.subplot(nbfig,nbdata,4)
plt.plot(vars()[datalist2[0]], vars()[computedlist[1]], 'b')
plt.grid(True)
plt.axis([min(vars()[datalist2[0]]), max(vars()[datalist2[0]]),
min(vars()[computedlist[1]]),max(vars()[computedlist[1]])])
plt.ylabel('Transport',fontsize='x-large')

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_trpsig.png')
#plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_trpsig.jpg',format='jpg')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Works with ORCA025 and NATL12
# 2) quite flexible
# 



