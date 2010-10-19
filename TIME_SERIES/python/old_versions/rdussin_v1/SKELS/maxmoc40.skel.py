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

f=open(datadir + 'CCOONNFFIIGG-CCAASSEE_maxmoc40.mtl','r')
lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
f.close()

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

datalist = ['year', 'mocg40n', 'mocg30s', 'moca40n', 'moca30s', 'mocip30s', 'mocip50s']
listplot = ['year', 'mocg40n', 'mocg30s', 'moca40n', 'moca30s', 'mocip30s', 'mocip50s']
titleplot= ['MOC at 40N','MOC at 30S','MOC at 40N','MOC at 30S','MOC at 30S','MOC at 50S']
listylabel=['Global', 'Atlantic', 'Indo-Pacific']
nbzone=3
nbfig=2
fig_size =  [15.,20.]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "maxmoc40.py : use default values (for global)"
elif config.find('NATL') == 0 :
	datalist = ['year', 'moca40n', 'moca30s']
	listplot = ['year', 'moca40n', 'moca30s']
	titleplot=['MOC at 40N', 'MOC at 30S']
	listylabel=['Atlantic']
	nbzone=1
	nbfig=2
	fig_size =  [10.,6.]
	print "maxmoc40.py : use custom values for NATL configs"
else :
	print "maxmoc40.py : Your config is not supported..."
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

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_maxmoc40.jpg',format='jpg')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Works with ORCA025 and NATL12
# 2) quite flexible
# 
