import matplotlib.pylab as plt
import Scientific.IO.NetCDF
import os
import Numeric as Numeric

from Numeric import *
from Scientific.IO import NetCDF
from NetCDF import *

import monitor_func as mf

###############################################################################################
### DRAKKAR FILE HIERARCHY
###############################################################################################

[config, case, frstyear, lastyear, datadir, plotdir ] = mf.get_drakkar_env()
plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

listfiles = ['01_BERING', '02_FRAM', '03_BAFFIN', '04_DENMARK_STRAIT', '05_ICELAND_SCOTLAND',
             '06_CUBA_FLORIDA', '07_FLORIDA_BAHAMAS', '08_DRAKE', '09_AUS_AA',
             '10_ITF', '11_MOZAMBIQUE_CHANNEL']

name = ['01_BERING', '02_FRAM', '03_BAFFIN', '04_DENMARK_STRAIT', '05_ICELAND_SCOTLAND',
             '06_CUBA_FLORIDA', '07_FLORIDA_BAHAMAS', '08_DRAKE', '09_AUS_AA',
             '10_ITF', '11_MOZAMBIQUE_CHANNEL']

datalist = ['mass' , 'heat', 'salt']
fig_size = [17.,25.]

sens=array([-1, 1, 1, 1, 1, 1, -1, 1, 1, -1 , 1], Float)

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "transports1.py : use default values (for global)"
elif config.find('NATL') == 0 :
	#sens=array([1, 1, 1, 1, 1, -1 ], Float)
	print "transports1.py : use custom values for NATL configs"
elif config.find('PERIANT') == 0 :
        sens=array([1, 1, 1, 1, 1, -1, -1, -1], Float)
        print "transports1.py : use custom values for PERIANT configs"
else :
	print "transports1.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

fichtable = open(datadir + '/' + 'drakkar_sections_table.txt','r')
sections=[sections for sections in fichtable.readlines() if sections.find('#') != 0 ] # remove empty lines
fichtable.close()


nsection=len(listfiles)

fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles[0] + '_transports.nc'
year = mf.get_time_counter(fich, frstyear)

for k in range(len(datalist)) :
	vars()[datalist[k]]=[]

for k in range(len(listfiles)) :
	fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles[k] + '_transports.nc'
        lines=[lines for lines in sections if lines.find(name[k]) == 0 ]
        sec=lines[0].split()[1]
	mass.append(mf.read_timeserie1d(fich, 'vtrp' + '_' + sec))
	heat.append(mf.read_timeserie1d(fich, 'htrp' + '_' + sec))
	salt.append(mf.read_timeserie1d(fich, 'strp' + '_' + sec))

mass = reshape(mass, [nsection, len(year)])
heat = reshape(heat, [nsection, len(year)])
salt = reshape(salt, [nsection, len(year)])

bigsens=transpose(sens * ones((len(year),nsection)))

massplt=mass * bigsens
heatplt=heat * bigsens
saltplt=salt * bigsens

###############################################################################################
### PLOTS part 1 : cosmetics (depends on the config)
###############################################################################################

params = {'axes.labelsize': 12,
          'axes.format' : '%.3f',
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
	plt.plot(year, massplt[k-1,:], 'r.-')
	plt.axis([min(year),max(year),min(massplt[k-1,:]),max(massplt[k-1,:])])
	plt.grid(True)
	plt.ylabel(name[k-1],fontsize='small')
	if k==1 :
		plt.title('Mass Transport',fontsize='large')
	plt.subplot(nsection,3,3*(k-1)+2)
	plt.plot(year, heatplt[k-1,:], 'r.-')
	plt.axis([min(year),max(year),min(heatplt[k-1,:]),max(heatplt[k-1,:])])
	plt.grid(True)
	if k==1 :
		plt.title('Heat Transport',fontsize='large')
	plt.subplot(nsection,3,3*(k-1)+3)
	plt.plot(year, saltplt[k-1,:], 'r.-')
	plt.axis([min(year),max(year),min(saltplt[k-1,:]),max(saltplt[k-1,:])])
	plt.grid(True)
	if k==1 :
		plt.title('Salt Transport',fontsize='large')


###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_transports1.png')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) not happy with the graphics
# 
