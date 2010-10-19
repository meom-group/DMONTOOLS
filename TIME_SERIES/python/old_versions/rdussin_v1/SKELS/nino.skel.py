import matplotlib.pyplot as plt

###############################################################################################
### DRAKKAR FILE HIERARCHY
###############################################################################################

config='CCOONNFFIIGG'
datadir='CCOONNFFIIGG/CCOONNFFIIGG-CCAASSEE-MONITOR/'
dataobsdir='MONITOR_PY/DATA_obs/'
plotdir='CCOONNFFIIGG/PLOTS/CCOONNFFIIGG-CCAASSEE/TIME_SERIES/'

###############################################################################################
### GLOBAL READING OF THE MTL FILES
###############################################################################################

f1=open(datadir + 'CCOONNFFIIGG-CCAASSEE_nino.mtl','r')
lignes1=[lignes1 for lignes1 in f1.readlines() if lignes1.strip() ] # remove empty lines
f1.close()

f2=open(dataobsdir + 'nino_obs.txt','r')
lignes2=[lignes2 for lignes2 in f2.readlines() if lignes2.strip() ] # remove empty lines
f2.close()

f3=open(dataobsdir + 'SOIindex.txt','r')
lignes3=[lignes3 for lignes3 in f3.readlines() if lignes3.strip() ] # remove empty lines
f3.close()

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

datalist=['year','month','nino1','unused1','nino2','unused2','nino3','unused3','nino4','unused4']
dataobslist1=['yearobs','monthobs','nino1obs','unused5','nino2obs','unused6','nino3obs','unused7','nino4obs','unused8']
dataobslist2=['datSOI','SOI']
#listplot=['nino1','nino2','nino3','nino4','SOI']
fig_size =  [15.,18.]
nmonth=12

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "nino.py : use default values (for global)"
else :
	print "nino.py : Your config is not supported..."
	print "The monitoring will use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

for k in range(len(datalist)) :
	vars()[datalist[k]]=[]

for k in range(len(dataobslist1)) :
	vars()[dataobslist1[k]]=[]

for k in range(len(dataobslist2)) :
	vars()[dataobslist2[k]]=[]

###############################################################################################
### DATA MANIPULATION
###############################################################################################

for chaine in lignes1 :
	element=chaine.split()
	vars()[datalist[0]].append(float(element[0]) + (float(element[1]) -1)/nmonth )
	for k in range(1,len(datalist)) :
		vars()[datalist[k]].append(float(element[k]))
	
for chaine in lignes2 :
	element=chaine.split()
	vars()[dataobslist1[0]].append(float(element[0]) + (float(element[1]) -1)/nmonth )
	for k in range(1,len(dataobslist1)) :
		vars()[dataobslist1[k]].append(float(element[k]))

for chaine in lignes3 :
	element=chaine.split()
	for k in range(1,nmonth+1) :
		vars()[dataobslist2[0]].append(float(element[0]) + ((float(k)-1)/nmonth) )
	for k in range(1,len(element)) :
		vars()[dataobslist2[1]].append(float(element[k]))

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

plt.subplot(5,1,1)
plt.plot(year, nino1, 'k', yearobs, nino1obs, 'g')
plt.axis([min(year), max(max(year),max(yearobs)), min(min(nino1),min(nino1obs)), max(max(nino1),max(nino1obs))])
plt.grid(True)
plt.ylabel('Nino1+2', fontsize='small')

plt.subplot(5,1,2)
plt.plot(year, nino2, 'k', yearobs, nino2obs, 'g')
plt.axis([min(year), max(max(year),max(yearobs)), min(min(nino2),min(nino2obs)), max(max(nino2),max(nino2obs))])
plt.grid(True)
plt.ylabel('Nino3', fontsize='small')

plt.subplot(5,1,3)
plt.plot(year, nino3, 'k', yearobs, nino3obs, 'g')
plt.axis([min(year), max(max(year),max(yearobs)), min(min(nino3),min(nino3obs)), max(max(nino3),max(nino3obs))])
plt.grid(True)
plt.ylabel('Nino4', fontsize='small')


plt.subplot(5,1,4)
plt.plot(year, nino4, 'k', yearobs, nino4obs, 'g')
plt.axis([min(year), max(max(year),max(yearobs)), min(min(nino4),min(nino4obs)), max(max(nino4),max(nino4obs))])
plt.grid(True)
plt.ylabel('Nino3.4', fontsize='small')

plt.subplot(5,1,5)
plt.plot(datSOI, SOI, 'b')
plt.axis([min(year), max(max(datSOI),max(year)), min(SOI), max(SOI)])
plt.grid(True)
plt.ylabel('SO Index', fontsize='small')

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir + 'CCOONNFFIIGG-CCAASSEE_nino.jpg', format='jpg')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Works with ORCA025
# 2) plots commands can be more compact
# 
