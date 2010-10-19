import matplotlib.pylab as plt
from Numeric import *

###############################################################################################
### MY FILE HIERARCHY
###############################################################################################

stockdir='/stock0/stock/dufour/'
homedata='/home/ciment/dufour/PLOT_MONITOR/'

###############################################################################################
### DRAKKAR FILE HIERARCHY
###############################################################################################

config='CCOONNFFIIGG'
datadir=stockdir + 'CCOONNFFIIGG/CCOONNFFIIGG-CCAASSEE-MONITOR/'
dataobsdir=homedata + 'MONITOR_PY/trunk/DATA_obs/'
plotdir=stockdir + 'CCOONNFFIIGG/PLOTS/CCOONNFFIIGG-CCAASSEE/TIME_SERIES/'

###############################################################################################
### GLOBAL READING OF THE MTL FILES
###############################################################################################

f=open(datadir + 'CCOONNFFIIGG-CCAASSEE_icemonth.mtl','r')
lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
f.close()

if config.find('ORCA') == 0 :
  f2=open(dataobsdir + 'North.mtl','r')
  lignes2=[lignes2 for lignes2 in f2.readlines() if lignes2.strip() ] # remove empty lines
  f2.close()

f3=open(dataobsdir + 'South.mtl','r')
lignes3=[lignes3 for lignes3 in f3.readlines() if lignes3.strip() ] # remove empty lines
f3.close()

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

datalist1=['year','varc','vant','aarc','aant','earc','eant']
# ICE VOLUME ARCTIC & ANTARCTIC, ICE AREA ARCTIC & ANTARCTIC, ICE EXTENT NSIDC ARCTIC & ANTARCTIC
datalist2=['yearobs','nedata','nadata']
# ARCTIC EXTENT AND AREA FROM DATA
datalist3=['yearobs2','sedata', 'sadata']
# ANTARCTIC EXTENT AND AREA FROM DATA

listplot1=['earc','aarc','eant','aant']
listplot2=['nedata','nadata','sedata','sadata']
listylabel=['Arctic Extent', 'Arctic Area', 'Antarctic Extent', 'Antarctic Area']

nbfig=max(len(listplot1),len(listplot2))
fig_size =  [15.,15.]
nmonth=12
convfactor=1000

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "icenoaa.py : use default values (for global)"
elif config.find('PERIANT') == 0 :
        datalist1=['year','vant','aant','eant']
        datalist3=['yearobs2','sedata', 'sadata']
        listplot1=['eant','aant']
        listplot2=['sedata','sadata']
        listylabel=['Antarctic Extent', 'Antarctic Area']
        nbfig=max(len(listplot1),len(listplot2))
        fig_size =  [15.,15.]
        print "icenoaa.py : use custom values for PERIANT configs" 
else :
	print "icenoaa.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

for k in range(len(datalist1)) :
	vars()[datalist1[k]]=[]

if config.find('ORCA') == 0 :
  for k in range(len(datalist2)) :
	vars()[datalist2[k]]=[]

for k in range(len(datalist3)) :
	vars()[datalist3[k]]=[]

###############################################################################################
### DATA MANIPULATION
###############################################################################################

for chaine in lignes[3:] :
	element=chaine.split()
        if config.find('ORCA') == 0 :
	  for k in range(1,1+nmonth) :
		vars()[datalist1[0]].append(float(element[0]) + ((float(k)-1)/nmonth) )
		vars()[datalist1[1]].append(float(element[k]))
	  for k in range( 1+nmonth,1+(2*nmonth) ) :
		vars()[datalist1[2]].append(float(element[k]))
	  for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
		vars()[datalist1[3]].append(float(element[k])/convfactor)
	  for k in range( 1+(3*nmonth),1+(4*nmonth) ) :
		vars()[datalist1[4]].append(float(element[k])/convfactor)
	  for k in range( 1+(4*nmonth),1+(5*nmonth) ) :
		vars()[datalist1[5]].append(float(element[k])/convfactor)
          for k in range( 1+(5*nmonth),1+(6*nmonth) ) :
		vars()[datalist1[6]].append(float(element[k])/convfactor)

        elif config.find('PERIANT') == 0 :
          for k in range(1,1+nmonth) :
                vars()[datalist1[0]].append(float(element[0]) + ((float(k)-1)/nmonth) )
                vars()[datalist1[1]].append(float(element[k]))
          for k in range( 1+nmonth,1+(2*nmonth) ) :
                vars()[datalist1[2]].append(float(element[k])/convfactor)
          for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                vars()[datalist1[3]].append(float(element[k])/convfactor)


if config.find('ORCA') == 0 :
  for chaine in lignes2[0:] :
	element=chaine.split()
	vars()[datalist2[0]].append(float(element[0]) + ((float(element[1])-1)/nmonth) )
	vars()[datalist2[1]].append(float(element[2]))
	vars()[datalist2[2]].append(float(element[3]))


for chaine in lignes3[0:] :
	element=chaine.split()
	vars()[datalist3[0]].append(float(element[0]) + ((float(element[1])-1)/nmonth ) )
	vars()[datalist3[1]].append(float(element[2]))
	vars()[datalist3[2]].append(float(element[3]))


# Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after ( Change of satellite SSM/R SSM/I )
if config.find('ORCA') == 0 :
  indN=vars()[datalist2[0]].index(1987.5)

  for k in range(indN) :
	vars()[datalist2[2]][k]=vars()[datalist2[2]][k]+1.19

  for k in range(indN,len(datalist2[0])) :
	vars()[datalist2[2]][k]=vars()[datalist2[2]][k]+0.31

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

minvalx=[1991, 1991, 1991, 1991]
maxvalx=[2000, 2000, 2000, 2000]
minvaly=[0, 0, 0, 0]
maxvaly=[20, 20, 20, 20]

###############################################################################################
### PLOTS part 2 : general script for plotting
###############################################################################################

if config.find('ORCA') == 0 :
  for k in range(nbfig) :
	plt.subplot(nbfig,1,k+1)
	plt.plot(vars()[datalist1[0]], vars()[listplot1[k]], 'k',
	vars()[datalist2[0]], vars()[listplot2[k]], 'b')
        plt.axis([minvalx[k], maxvalx[k], minvaly[k], maxvaly[k]])
	plt.grid(True)
	plt.ylabel(listylabel[k],fontsize='large')

elif config.find('PERIANT') == 0 :
  for k in range(nbfig) :
        plt.subplot(nbfig,1,k+1)
        plt.plot(vars()[datalist1[0]], vars()[listplot1[k]], 'k',
        vars()[datalist3[0]], vars()[listplot2[k]], 'b')
        plt.axis([minvalx[k], maxvalx[k], minvaly[k], maxvaly[k]])
        plt.grid(True)
        plt.ylabel(listylabel[k],fontsize='large')


###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_sea_icenoaa.png')
#plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_sea_icenoaa.jpg', format='jpg')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Works with ORCA025 and probably not NATL12
# 2) bug found in monitor matlab where indN was 92 and corresponds to june 86 
# 
