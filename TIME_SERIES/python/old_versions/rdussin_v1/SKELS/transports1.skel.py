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

f=open(datadir + 'CCOONNFFIIGG-CCAASSEE_matrix.mtl','r')
lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
f.close()

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

datalist=['year','name','mass','heat','salt','sshmean','tmean','smean']
fig_size =  [15.,20.]
nsection=11
sens=array([-1, 1, 1, 1, 1, 1, -1, 1, 1, -1 , 1], Float)

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "transports1.py : use default values (for global)"
elif config.find('NATL') == 0 :
	nsection=6
	sens=array([1, 1, 1, 1, 1, -1 ], Float)
	print "transports1.py : use custom values for NATL configs"
elif config.find('PERIANT') == 0 :
        nsection=8
        sens=array([1, 1, 1, 1, 1, -1, -1, -1], Float)
        print "transports1.py : use custom values for PERIANT configs"
else :
	print "transports1.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

for k in range(len(datalist)) :
	vars()[datalist[k]]=[]

###############################################################################################
### DATA MANIPULATION
###############################################################################################

for chaine in lignes[1:2] :
	element=chaine.split()
	for k in range(3,3+nsection) :
		tempname=element[k]
		tempname=tempname.replace('_','-')
		name.append(tempname[3:])

for chaine in lignes[3:] :
	element=chaine.split()
	year.append(float(element[0]))
	for k in range(1,1+nsection) :
		mass.append(float(element[k]))
	for k in range( 1+nsection,1+(2*nsection) ) :
		heat.append(float(element[k]))
	for k in range( 1+(2*nsection),1+(3*nsection) ) :
		salt.append(float(element[k]))
	sshmean.append(float(element[1+(3*nsection)+0]))
	tmean.append(float(element[1+(3*nsection)+1]))
	smean.append(float(element[1+(3*nsection)+2]))

mass=reshape(mass, (-1,nsection))
heat=reshape(heat, (-1,nsection))
salt=reshape(salt, (-1,nsection))
bigsens=ones((size(year),1,)) * sens
massplt=mass * bigsens
heatplt=heat * bigsens
saltplt=salt * bigsens

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

for k in range(1,nsection+1) :
	plt.subplot(nsection,3,3*(k-1)+1)
	plt.plot(year, massplt[:,k-1], 'r.-')
	plt.axis([min(year),max(year),min(massplt[:,k-1]),max(massplt[:,k-1])])
	plt.grid(True)
	plt.ylabel(name[k-1],fontsize='x-large')
	if k==1 :
		plt.title('Mass Transport',fontsize='large')
	plt.subplot(nsection,3,3*(k-1)+2)
	plt.plot(year, heatplt[:,k-1], 'r.-')
	plt.axis([min(year),max(year),min(heatplt[:,k-1]),max(heatplt[:,k-1])])
	plt.grid(True)
	if k==1 :
		plt.title('Heat Transport',fontsize='large')
	plt.subplot(nsection,3,3*(k-1)+3)
	plt.plot(year, saltplt[:,k-1], 'r.-')
	plt.axis([min(year),max(year),min(saltplt[:,k-1]),max(saltplt[:,k-1])])
	plt.grid(True)
	if k==1 :
		plt.title('Salt Transport',fontsize='large')


###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_transports1.png')
#plt.savefig(plotdir + 'PERIANT05-GCdmpH3_transports1.jpg', format='jpg')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Works with ORCA025 and NATL12
# 2) lot a stuff hardcoded
# 
