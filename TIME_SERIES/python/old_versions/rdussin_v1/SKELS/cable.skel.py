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
case='CCAASSEE'
datadir=stockdir + 'CCOONNFFIIGG/CCOONNFFIIGG-CCAASSEE-MONITOR/'
dataobsdir='MONITOR_PY/trunk/DATA_obs/'
plotdir=stockdir + 'CCOONNFFIIGG/PLOTS/CCOONNFFIIGG-CCAASSEE/TIME_SERIES/'

###############################################################################################
### GLOBAL READING OF THE MTL FILES
###############################################################################################

f1=open(datadir + 'CCOONNFFIIGG-CCAASSEE_matrix.mtl','r')
lignes1=[lignes1 for lignes1 in f1.readlines() if lignes1.strip() ] # remove empty lines
f1.close()

f2=open(dataobsdir + 'cable.mtl','r')
lignes2=[lignes2 for lignes2 in f2.readlines() if lignes2.strip() ] # remove empty lines
f2.close()

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

datalist=['year','yearobs','name','mass','heat','salt','sshmean','tmean','smean','cable']
fig_size =  [5.,5.]
nsection=11
indsection=6

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "cable.py : use default values (for global)"
elif config.find('NATL') == 0 :
	nsection=6
	indsection=5
	print "cable.py : use custom values for NATL configs"
elif config.find('PERIANT') == 0 :
        nsection=8
        indsection=3
        print "cable.py : use custom values for PERIANT configs"
else :
	print " cable.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

for k in range(len(datalist)) :
	vars()[datalist[k]]=[]

###############################################################################################
### DATA MANIPULATION
###############################################################################################

for chaine in lignes1[1:2] :
	element=chaine.split()
	for k in range(3,3+nsection) :
		tempname=element[k]
		tempname=tempname.replace('_','-')
		name.append(tempname[3:])

for chaine in lignes1[3:] :
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

for chaine in lignes2[:] :
	element=chaine.split()
	yearobs.append(float(element[0]))
	cable.append(float(element[1]))

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

plt.plot(year,(-1.*mass[:,indsection]),'r.-',yearobs,cable,'b.-')
plt.axis([min(year),max(max(year),max(yearobs)),min(min(-1.*mass[:,indsection]),min(cable)), 
max(max(-1.*mass[:,indsection]),max(cable))])
plt.grid(True)
plt.title('Mass Transport',fontsize='small')
plt.ylabel(name[indsection],fontsize='small')
#plt.subtitle(config + case, fontsize=14)

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_cable.png')
#plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_cable.jpg', format='jpg')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Works with ORCA025 and NATL12
# 2) NaN in obs gives an interpolated points on the figure -> needs to be fixed
# 3) variables are hardcoded -> not very flexible
