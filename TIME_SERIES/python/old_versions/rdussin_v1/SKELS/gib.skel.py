import matplotlib.pyplot as plt
from Numeric import *

###############################################################################################
### DRAKKAR FILE HIERARCHY
###############################################################################################

config='CCOONNFFIIGG'
datadir='CCOONNFFIIGG/CCOONNFFIIGG-CCAASSEE-MONITOR/'
plotdir='CCOONNFFIIGG/PLOTS/CCOONNFFIIGG-CCAASSEE/TIME_SERIES/'

###############################################################################################
### GLOBAL READING OF THE MTL FILES
###############################################################################################

f=open(datadir + 'CCOONNFFIIGG-CCAASSEE_gib.mtl','r')
lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
f.close()

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

datalist=['year','levels','tlev','slev','tmodel','smodel']
fig_size =  [15.,18.]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "gib.py : use default values (for global)"
else :
	print "gib.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

for k in range(len(datalist)) :
	vars()[datalist[k]]=[]

###############################################################################################
### DATA MANIPULATION
###############################################################################################

for chaine in lignes[0:1] : # this line contains levels depth
	element=chaine.split()
	for k in range(1, len(element)) :
		vars()[datalist[1]].append(-1*(float(element[k])))

for chaine in lignes[1:2] : # temperature levitus
	element=chaine.split()
	for k in range(1, len(element)) :
		vars()[datalist[2]].append(float(element[k]))

for chaine in lignes[2:3] : # salinity levitus
	element=chaine.split()
	for k in range(1, len(element)) :
		vars()[datalist[3]].append(float(element[k]))

for chaine in lignes[3::2] : # temperature in model
	element=chaine.split()
	vars()[datalist[0]].append(float(element[0]))
	for k in range(1, len(element)) :	
		vars()[datalist[4]].append(float(element[k]))


for chaine in lignes[4::2] : # salinity in model
	element=chaine.split()
	for k in range(1, len(element)) :	
		vars()[datalist[5]].append(float(element[k]))


tmodel=reshape(tmodel, (-1,len(levels)) )
smodel=reshape(smodel, (-1,len(levels)) )
tlev2D = ones((size(year),1,)) * tlev
slev2D = ones((size(year),1,)) * slev

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

ncontour=25

###############################################################################################
### PLOTS part 2 : general script for plotting
###############################################################################################

plt.subplot(3,2,1)
plt.plot(tlev, levels, 'b', tmodel[-1,:], levels, 'r')
plt.grid(True)
plt.axis([min(min(tmodel[-1,:]),min(tlev)), max(max(tmodel[-1,:]),max(tlev)),min(levels),max(levels)])
plt.ylabel('Depth')
plt.title('Last Tgib profile 14W-10W, 36N-40N (blue is Levitus)')

plt.subplot(3,2,2)
plt.contourf(year, levels, transpose(tmodel-tlev2D),ncontour)
plt.colorbar()
plt.grid(True)
plt.ylabel('Temperature anomaly')

plt.subplot(3,2,3)
plt.plot(slev, levels, 'b', smodel[-1,:], levels, 'r')
plt.grid(True)
plt.axis([min(min(smodel[-1,:]),min(slev)), max(max(smodel[-1,:]),max(slev)),min(levels),max(levels)])
plt.ylabel('Depth')
plt.title('Last Sgib profile 14W-10W, 36N-40N (blue is Levitus)')

plt.subplot(3,2,4)
plt.contourf(year, levels, transpose(smodel-slev2D),ncontour)
plt.colorbar()
plt.grid(True)
plt.ylabel('Salinity anomaly')

plt.subplot(3,2,5)
plt.plot(year, (tmodel[:,24]-tlev[24]), 'r.-')
plt.grid(True)
plt.axis([min(year), max(year), min(tmodel[:,24]-tlev[24]), max(tmodel[:,24]-tlev[24])])
plt.ylabel('T anom (1000m)')
plt.title('Temperature Anomaly Level 25')

plt.subplot(3,2,6)
plt.plot(year, (smodel[:,24]-slev[24]), 'r.-')
plt.grid(True)
plt.axis([min(year), max(year), min(smodel[:,24]-slev[24]), max(smodel[:,24]-slev[24])])
plt.ylabel('S anom (1000m)')
plt.title('Salinity Anomaly Level 25')

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_gib.jpg', format='jpg')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Should work with every config that has a gib.mtl output (obviously)
# 2) lot of variables are hardcoded -> not very flexible
# 3) plots script could be more compact -> create corresponding arrays
# 4) I guess the data in the mtl will probably not evolve so the actual script may not need
#    more flexibility
