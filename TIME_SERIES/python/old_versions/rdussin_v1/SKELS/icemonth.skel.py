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

f=open(datadir + 'CCOONNFFIIGG-CCAASSEE_icemonth.mtl','r')
lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
f.close()

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

datalist=['year','varc','vant','aarc','aant','earc','eant']
# ICE VOLUME ARCTIC AND ANTARCTIC
# ICE AREA ARCTIC AND ANTARCTIC
# ICE EXTENT NSIDC ARCTIC AND ANTARCTIC
nmonth=12
fig_size =  [15.,15.]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
        test = 2
	print "icemonth.py : use default values (for global)"
elif config.find('PERIANT') == 0 :
        test = 1
        datalist=['year','vant','aant','eant']
        fig_size =  [15.,6.]
        print "icemonth.py : use custom values for PERIANT configs"
else :
	print "icemonth.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

for k in range(len(datalist)) :
	vars()[datalist[k]]=[]

###############################################################################################
### DATA MANIPULATION
###############################################################################################

for chaine in lignes[3:] :
	element=chaine.split()
        if config.find('ORCA') == 0 :
	  for k in range(1,1+nmonth) :
		year.append(float(element[0]) + ((float(k)-1)/nmonth) )
		varc.append(float(element[k]))
	  for k in range( 1+nmonth,1+(2*nmonth) ) :
		vant.append(float(element[k]))
	  for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
		aarc.append(float(element[k]))
	  for k in range( 1+(3*nmonth),1+(4*nmonth) ) :
		aant.append(float(element[k]))
	  for k in range( 1+(4*nmonth),1+(5*nmonth) ) :
		earc.append(float(element[k]))
	  for k in range( 1+(5*nmonth),1+(6*nmonth) ) :
		eant.append(float(element[k]))

        elif config.find('PERIANT') == 0 :
          for k in range(1,1+nmonth) :
                year.append(float(element[0]) + ((float(k)-1)/nmonth) )
                vant.append(float(element[k]))
          for k in range( 1+nmonth,1+(2*nmonth) ) :
                aant.append(float(element[k]))
          for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                eant.append(float(element[k]))

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

if config.find('ORCA') == 0 :
  plt.subplot(2,3,1)
  plt.plot(year, varc, 'k')
  plt.axis([min(year), max(year), 0, 60000])
  plt.grid(True)
  plt.ylabel('Northern',fontsize='large')
  plt.title('Ice Volume (10**9 m**3)',fontsize='large')

  plt.subplot(2,3,2)
  plt.plot(year, aarc, 'k')
  plt.grid(True)
  plt.axis([min(year), max(year), 0, 15000])
  plt.title('Ice Area (10**9 m**2)',fontsize='large')

  plt.subplot(2,3,3)
  plt.plot(year, earc, 'k')
  plt.grid(True)
  plt.axis([min(year), max(year), 0, 20000])
  plt.title('Ice extent (10**9 m**2)',fontsize='large')

if (config.find('ORCA') == 0) + (config.find('PERIANT') == 0) :  
  plt.subplot(test,3,test*test)
  plt.plot(year, vant, 'k')
  plt.grid(True)
  plt.axis([min(year), max(year), 0, 20000])
  plt.ylabel('Southern',fontsize='large')
  if test == 1 :
    plt.title('Ice Volume (10**9 m**3)',fontsize='large')

  plt.subplot(test,3,test*test+1)
  plt.plot(year, aant, 'k')
  plt.grid(True)
  plt.axis([min(year), max(year), 0, 20000])
  if test == 1 :
    plt.title('Ice Area (10**9 m**2)',fontsize='large')

  plt.subplot(test,3,test*3)
  plt.plot(year, eant, 'k')
  plt.grid(True)
  plt.axis([min(year), max(year), 0, 20000])
  if test == 1 :
    plt.title('Ice extent (10**9 m**2)',fontsize='large')

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_icemonth.png')
#plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_icemonth.jpg', format='jpg')


###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) FOR NATL, create case in config selector with no antarctic (no mtl available) 
# 2) compact plots possible, data manipulation not flexible -> will fail on NATL
