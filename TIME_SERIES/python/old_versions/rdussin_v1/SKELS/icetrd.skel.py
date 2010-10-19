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
datalist2=['yearobsmarch','nedatamarch','nadatamarch','yearobssept','nedatasept','nadatasept']
datalist3=['yearobs2march','sedatamarch','sadatamarch','yearobs2sept','sedatasept','sadatasept']
month1=3
month2=9
fig_size =  [15.,15.]
nmonth=12
convfactor=1000

#first plot
listplot1=[]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
        test = 2
	print "icetrd.py : use default values (for global)"
elif config.find('PERIANT') == 0 :
        test = 1
        datalist1=['year','vant','aant','eant']
        datalist3=['yearobs2march','sedatamarch','sadatamarch','yearobs2sept','sedatasept','sadatasept']
        fig_size =  [15.,6.]
        print "icetrd.py : use custom values for PERIANT configs"
else :
	print "icetrd.py : Your config is not supported..."
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
		vars()[datalist1[1]].append(float(element[k])/convfactor)
	  for k in range( 1+nmonth,1+(2*nmonth) ) :
		vars()[datalist1[2]].append(float(element[k])/convfactor)
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
                vars()[datalist1[1]].append(float(element[k])/convfactor)
          for k in range( 1+nmonth,1+(2*nmonth) ) :
                vars()[datalist1[2]].append(float(element[k])/convfactor)
          for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                vars()[datalist1[3]].append(float(element[k])/convfactor)

if config.find('ORCA') == 0 :
  for chaine in lignes2[0:] :
	element=chaine.split()
	if float(element[1]) == month1 :
		vars()[datalist2[0]].append(float(element[0]))
		vars()[datalist2[1]].append(float(element[2]))
		vars()[datalist2[2]].append(float(element[3]))
	if float(element[1]) == month2 :
		vars()[datalist2[3]].append(float(element[0]))
		vars()[datalist2[4]].append(float(element[2]))
		vars()[datalist2[5]].append(float(element[3]))

for chaine in lignes3[0:] :
	element=chaine.split()
	if float(element[1]) == month1 :
		vars()[datalist3[0]].append(float(element[0]))
		vars()[datalist3[1]].append(float(element[2]))
		vars()[datalist3[2]].append(float(element[3]))
	if float(element[1]) == month2 :
		vars()[datalist3[3]].append(float(element[0]))
		vars()[datalist3[4]].append(float(element[2]))
		vars()[datalist3[5]].append(float(element[3]))


# Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after ( Change of satellite SSM/R SSM/I )
if config.find('ORCA') == 0 :
  indN=vars()[datalist2[0]].index(1987)

  for k in range(indN+1) :
	vars()[datalist2[2]][k]=vars()[datalist2[2]][k]+1.19

  for k in range(indN+1,len(datalist2[0])) :
	vars()[datalist2[2]][k]=vars()[datalist2[2]][k]+0.31
	
  for k in range(indN) :
	vars()[datalist2[5]][k]=vars()[datalist2[5]][k]+1.19

  for k in range(indN,len(datalist2[0])) :
	vars()[datalist2[5]][k]=vars()[datalist2[5]][k]+0.31


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
  plt.plot(year[::12], varc[2::12], 'k.-')
  #plt.axis([1950, 2010, 0, 20])
  plt.grid(True)
  plt.title('Volume Arctic March',fontsize='large')

  plt.subplot(2,3,2)
  plt.plot(year[::12], aarc[2::12], 'k.-',yearobsmarch, nadatamarch, 'b.-')
  plt.grid(True)
  #plt.axis([1950, 2010, 0, 15])
  plt.title('Area Arctic March',fontsize='large')

  plt.subplot(2,3,3)
  plt.plot(year[::12], earc[2::12], 'k.-',yearobsmarch, nedatamarch, 'b.-')
  plt.grid(True)
  #plt.axis([1950, 2010, 0, 30])
  plt.title('Extent Arctic March',fontsize='large')

if (config.find('ORCA') == 0) + (config.find('PERIANT') == 0) :
  plt.subplot(test,3,test*test)
  plt.plot(year[::12], vant[8::12], 'k.-')
  plt.grid(True)
  #plt.axis([1950, 2010, 0, 20])
  plt.title('Volume Antarctic September',fontsize='large')

  plt.subplot(test,3,test*test+1)
  plt.plot(year[::12], aant[8::12], 'k.-',yearobs2sept, sadatasept, 'b.-')
  plt.grid(True)
  #plt.axis([1950, 2010, 0, 30])
  plt.title('Area Antarctic September',fontsize='large')

  plt.subplot(test,3,test*3)
  plt.plot(year[::12], eant[8::12], 'k.-', yearobs2sept, sedatasept, 'b.-')
  plt.grid(True)
  #plt.axis([1950, 2010, 0, 20])
  plt.title('Extent Antarctic September',fontsize='large')

### SAVE THE FIGURE ###
plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_sea_icetrd.png')
#plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_sea_icetrd.jpg', format='jpg')

plt.close()

if config.find('ORCA') == 0 :
  plt.subplot(2,3,1)
  plt.plot(year[::12], varc[8::12], 'k.-')
  #plt.axis([1950, 2010, 0, 20])
  plt.grid(True)
  plt.title('Volume Arctic September',fontsize='large')

  plt.subplot(2,3,2)
  plt.plot(year[::12], aarc[8::12], 'k.-',yearobssept, nadatasept, 'b.-')
  plt.grid(True)
  #plt.axis([1950, 2010, 0, 15])
  plt.title('Area Arctic September',fontsize='large')

  plt.subplot(2,3,3)
  plt.plot(year[::12], earc[8::12], 'k.-',yearobssept, nedatasept, 'b.-')
  plt.grid(True)
  #plt.axis([1950, 2010, 0, 30])
  plt.ylabel('Extent Arctic September',fontsize='large')

if (config.find('ORCA') == 0) + (config.find('PERIANT') == 0) :
  plt.subplot(test,3,test*test)
  plt.plot(year[::12], vant[2::12], 'k.-')
  plt.grid(True)
  #plt.axis([1950, 2010, 0, 20])
  plt.title('Volume Antarctic March',fontsize='large')

  plt.subplot(test,3,test*test+1)
  plt.plot(year[::12], aant[2::12], 'k.-',yearobs2march, sadatamarch, 'b.-')
  plt.grid(True)
  #plt.axis([1950, 2010, 0, 30])
  plt.title('Area Antarctic March',fontsize='large')

  plt.subplot(test,3,test*3)
  plt.plot(year[::12], eant[2::12], 'k.-', yearobs2march, sedatamarch, 'b.-')
  plt.grid(True)
  #plt.axis([1950, 2010, 0, 20])
  plt.title('Extent Antarctic March',fontsize='large')

### SAVE THE FIGURE ###
plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_sea_icetrd_mini.png')
#plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_sea_icetrd_mini.jpg', format='jpg')

