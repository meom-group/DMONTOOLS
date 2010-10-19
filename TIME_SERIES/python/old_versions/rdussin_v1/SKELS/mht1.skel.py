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

f=open(datadir + 'CCOONNFFIIGG-CCAASSEE_heat.mtl','r')
lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
f.close()
###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

variable = ['Southward Accumulated Qnet','Advective MHT']
zone = ['Global ','Atlantic ','Indo-Pacific ']
nbvar=len(variable)
nbzone=len(zone)
nbzoneplt=nbzone
fig_size =  [15.,18.]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "mht1.py : use default values (for global)"
elif config.find('NATL') == 0 :
	zone = ['Global']
	nbzoneplt=1
	fig_size =  [15.,6.]
	print "mht1.py : use custom values for NATL configs"
elif config.find('PERIANT') == 0 :
        zone = ['Global']
        nbzoneplt=1
        fig_size =  [15.,6.]
        print "mht1.py : use custom values for PERIANT configs"
else :
	print "mht1.py : Your config is not supported..."
	print "The monitoring will use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

yearx=[]
lat=[]
alltab=[]

###############################################################################################
### DATA MANIPULATION
###############################################################################################

for chaine in lignes[0:1] :
	element=chaine.split()
	for k in range(1,len(element)) :
		lat.append(float(element[k]))

for chaine in lignes[1:] :
	element=chaine.split()
	yearx.append(float(element[0]))
	for k in range(1,len(element)) :
		alltab.append(float(element[k]))

alltab=reshape(alltab, (-1,len(lat)))
yearx=reshape(yearx, (-1,nbvar*nbzone))
year=yearx[:,0].copy()
MHTa=alltab[0::nbvar,:].copy()
MHTd=alltab[1::nbvar,:].copy()
MHTa=reshape(MHTa, (-1,len(lat)))
MHTd=reshape(MHTd, (-1,len(lat)))

###############################################################################################
### PLOTS part 1 : cosmetics (depends on the config)
###############################################################################################

params = {'axes.labelsize': 12,
          'text.fontsize': 12,
          'legend.fontsize': 12,
          'xtick.labelsize': 10,
          'ytick.labelsize': 10,
          'figure.figsize': fig_size}
plt.rcParams.update(params)

###############################################################################################
### PLOTS part 2 : general script for plotting
###############################################################################################

for k in range(1,nbzoneplt+1) :
	plt.subplot(nbzoneplt,nbvar+1,3*(k-1)+1)
	plt.contourf(year, lat, transpose(MHTd[k-1::nbzone,:]))
	plt.colorbar()
	plt.grid(True)
	plt.ylabel(zone[k-1],fontsize='x-large')
	if k==1 :
		plt.title(variable[0],fontsize='large')
	plt.subplot(nbzoneplt,nbvar+1,3*(k-1)+2)
	plt.contourf(year, lat, transpose(MHTa[k-1::nbzone,:]))
	plt.colorbar()
	plt.grid(True)
	if k==1 :
		plt.title(variable[1],fontsize='large')
	plt.subplot(nbzoneplt,nbvar+1,3*(k-1)+3)
	plt.contourf(year, lat, transpose(MHTd[k-1::nbzone,:] - MHTa[k-1::nbzone,:]))
	plt.colorbar()
	plt.grid(True)
	if k==1 :
		plt.title('Second minus first',fontsize='large')
		
###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_MHT1.png')
#plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_MHT1.jpg', format='jpg')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Works with ORCA025 and NATL12
# 2) in mtl we have arrays of zeros in NATL12 -> need to separate nbzone and nbzoneplt
# 3) some code tuning left to do
#
