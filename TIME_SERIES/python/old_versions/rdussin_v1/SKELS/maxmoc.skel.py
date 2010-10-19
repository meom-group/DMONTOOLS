import matplotlib.pyplot as plt

###############################################################################################
### DRAKKAR FILE HIERARCHY
###############################################################################################

config='CCOONNFFIIGG'
datadir='CCOONNFFIIGG/CCOONNFFIIGG-CCAASSEE-MONITOR/'
plotdir='CCOONNFFIIGG/PLOTS/CCOONNFFIIGG-CCAASSEE/TIME_SERIES/'

###############################################################################################
### GLOBAL READING OF THE MTL FILES
###############################################################################################

f=open(datadir + 'CCOONNFFIIGG-CCAASSEE_maxmoc.mtl','r')
lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
f.close()

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

datalist = ['year', 'mocglomax', 'mocglomin', 'mocglomht', 'mocatlmax', 'mocatlmin', 'mocatlmht',
'mocipn', 'mocips', 'unused1', 'mocausmax', 'mocausmin', 'unused2']
listplot = ['year', 'mocglomax', 'mocglomin', 'mocglomht', 'mocatlmax', 'mocatlmin', 'mocatlmht',
'mocipn', 'mocips', 'mocausmax', 'mocausmin']
titleplot= ['Max Overturning (20N-60N 500m-2000m)', 'Min Overturning (-40S 30N 2000m-5500m)',
'MHT a 20N', 'Max Overturning (0N-60N 500m-2000m)', 
'Min Overturning (-20S 40N 2000m-5500m)', 'MHT at 20N', 'Min Overturning (15N 50N 100m-1000m)',
'Min Overturning (-30S 20N 1000m-5500m)', '',
'Max Overturning (-70S 0S 0m-2000m)',  'Min Overturning (-70S 0S 2000m-5500m)', '']
listylabel=['Global', 'Atlantic', 'Indo-Pacific', 'Austral']
nbzone=4
nbfig=3
fig_size =  [15.,20.]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "maxmoc.py : use default values (for global)"
elif config.find('NATL') == 0 :
	datalist = ['year', 'mocatlmax', 'mocatlmin', 'mocatlmht', 'unused1', 'unused2', 'unused3']
	listplot = ['year', 'mocatlmax', 'mocatlmin', 'mocatlmht']
	titleplot=['Max Overturning (0N-60N 500m-2000m)', 'Min Overturning (-20S 40N 2000m-5500m)', 'MHT at 20N']
	nbzone=1
	nbfig=3
	fig_size =  [15.,6.]
	print "maxmoc.py : use custom values for NATL configs"
else :
	print "maxmoc.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

for k in range(len(datalist)) :
	vars()[datalist[k]]=[]

###############################################################################################
### DATA MANIPULATION
###############################################################################################

for chaine in lignes :
	element=chaine.split()
	for k in range(len(datalist)) :
		vars()[datalist[k]].append(float(element[k]))

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

for k in range(1,len(datalist)) :
	if datalist[k].find('unused') != 0 :
		plt.subplot(nbzone, nbfig, k)
		plt.plot(vars()[datalist[0]],vars()[datalist[k]],'r.-')
		plt.axis([min(vars()[datalist[0]]), max(vars()[datalist[0]]), 
		min(vars()[datalist[k]]), max(vars()[datalist[k]])])
		plt.grid(True)
		plt.title(titleplot[k-1],fontsize='small')
		if divmod(k,nbfig)[1] == 1 :
			plt.ylabel(listylabel[divmod(k,nbfig)[0]],fontsize='small')

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_maxmoc.jpg',format='jpg')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Works with ORCA025 and NATL12
# 2) quite flexible
# 
