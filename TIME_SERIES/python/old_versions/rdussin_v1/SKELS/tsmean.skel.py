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

f1=open(datadir + 'CCOONNFFIIGG-CCAASSEE_TMEAN.mtl','r')
lignes1=[lignes1 for lignes1 in f1.readlines() if lignes1.strip() ] # remove empty lines
f1.close()

f2=open(datadir + 'CCOONNFFIIGG-CCAASSEE_SMEAN.mtl','r')
lignes2=[lignes2 for lignes2 in f2.readlines() if lignes2.strip() ] # remove empty lines
f2.close()

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

fig_size =  [15.,18.]
datalist1 = ['unused1','unused2','unused3','levels'] # line 2 in TMEAN.mtl and SMEAN.mtl
datalist2 = ['year', 'sshmean', 'tmean', 'tmodel'] # following lines in TMEAN.mtl
datalist3 = ['year2', 'sshmean2','smean', 'smodel'] # following lines in SMEAN.mtl

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "tsmean.py : use default values (for global)"
elif config.find('NATL') == 0 :
	print "tsmean.py : use default values (suitable for NATL)"
elif config.find('PERIANT') == 0 :
        print "tsmean.py : use default values (suitable for PERIANT)"
else :
	print "tsmean.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

for k in range(len(datalist1)) :
	vars()[datalist1[k]]=[]

for k in range(len(datalist2)) :
	vars()[datalist2[k]]=[]

for k in range(len(datalist3)) :
	vars()[datalist3[k]]=[]

###############################################################################################
### DATA MANIPULATION
###############################################################################################

for chaine in lignes1[2:3] :
	element=chaine.split()
	for k in range(datalist1.index('levels'), len(element)) : # get levels depth
		levels.append(+1*(float(element[k])))

for chaine in lignes1[3:] :
	element=chaine.split()
	for k in range(0,datalist2.index('tmodel')) : # get single values before tmodel
		vars()[datalist2[k]].append(float(element[k]))
	for k in range(datalist2.index('tmodel'), len(element)) : # get tmodel in each level
		tmodel.append(float(element[k]))

for chaine in lignes2[3:] :
	element=chaine.split()
	for k in range(0,datalist3.index('smodel')) : # get single values before smodel
		vars()[datalist3[k]].append(float(element[k]))
	for k in range(datalist3.index('smodel'), len(element)) : # get smodel in each level
		smodel.append(float(element[k]))

tmodel=reshape(tmodel,(size(year),-1))
smodel=reshape(smodel,(size(year),-1))

###############################################################################################
### PLOTS part 1 : cosmetics (depends on the config)
###############################################################################################

params = {'axes.labelsize': 10,
          'text.fontsize': 10,
          'legend.fontsize': 10,
          'xtick.labelsize': 8,
          'ytick.labelsize': 8,
          'figure.figsize': fig_size}
plt.rcParams.update(params)

###############################################################################################
### PLOTS part 2 : general script for plotting
###############################################################################################

### 1) PLOTS OF 3D MEANS
list3Dmean = ['tmean', 'smean', 'sshmean']
titles3Dmean = ['3D Global T Mean', '3D Global S Mean', '3D Global SSH Mean']
nb3Dmean=len(list3Dmean)

for k in range(nb3Dmean) :
	plt.subplot(3,nb3Dmean,k+1)
	plt.plot(year, vars()[list3Dmean[k]], 'r.-')
	plt.grid(True)
	plt.axis([min(year), max(year),min(vars()[list3Dmean[k]]),max(vars()[list3Dmean[k]])])
	plt.title(titles3Dmean[k])

### 2) 2D PLOTS
ncontour=25

plt.subplot(3,nb3Dmean,nb3Dmean+1)
plt.semilogy((tmodel[-1,:]-tmodel[0,:]),levels,'r')
plt.grid(True)
plt.axis([min(tmodel[-1,:]-tmodel[0,:]), max(tmodel[-1,:]-tmodel[0,:]),min(levels),max(levels)])
plt.ylabel('Log Depth')
plt.title('Global change in T')

plt.subplot(3,nb3Dmean,nb3Dmean+2)
plt.contourf(year, log10(levels), transpose(tmodel[:,:]-tmodel[0,:]),ncontour)
plt.colorbar()
plt.grid(True)
plt.ylabel('Temperature anomaly')

plt.subplot(3,nb3Dmean,2*nb3Dmean+1)
plt.semilogy((smodel[-1,:]-smodel[0,:]),levels,'r')
plt.grid(True)
plt.axis([min(smodel[-1,:]-smodel[0,:]), max(smodel[-1,:]-smodel[0,:]),min(levels),max(levels)])
plt.ylabel('Log Depth')
plt.title('Global change in S')

plt.subplot(3,nb3Dmean,2*nb3Dmean+2)
plt.contourf(year, log10(levels), transpose(smodel[:,:]-smodel[0,:]),ncontour)
plt.colorbar()
plt.grid(True)
plt.ylabel('Salinity anomaly')

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_tsmean.png')
#plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_tsmean.jpg',format='jpg')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) An empty line will not be a problem but if data is missing in an existing line -> pb
# 2) Single data values before tmodel/smodel can be added/removed in the mtl provided 
#    corrections are done in datalist*
# 3) tmodel and smodel have to stay at the end of the lines in the mtl
# 4) suitable for every config, number of levels is not hard-coded
# 5) log and 2D year-depth plots are upside down, pb with y indices in 2D plots
#
