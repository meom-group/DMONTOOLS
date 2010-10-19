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

fich     = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + 'icemonth.nc'
datalist = ['NVolume', 'NArea','NExtent','SVolume', 'SArea','SExtent']

north = 1 # weither we plot or not the artic
south = 1 # weither we plot or not the antarctic

fig_size =  [15.,15.]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "icemonth.py : use default values (for global)"
elif config.find('PERIANT') == 0 :
        datalist=['SVolume', 'SArea','SExtent']
	north = 0
	south = 1
        fig_size =  [15.,6.]
        print "icemonth.py : use custom values for PERIANT configs"
else :
	print "icemonth.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### Read NetCDF files
###############################################################################################

year = mf.get_time_counter(fich, frstyear)

for k in range(len(datalist)) :
	vars()[datalist[k]] = mf.read_timeserie1d(fich, datalist[k])


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

nbplot = len(datalist)
nbzone = north + south
nbplotline = nbplot/nbzone # warning, we work with integer

###############################################################################################
### PLOTS part 2 : general script for plotting
###############################################################################################

if north == 1 :
	plt.subplot(nbzone,nbplotline,1)
	plt.plot(year, NVolume, 'k')
	plt.axis([min(year), max(year), 0, 60000])
	plt.grid(True)
	plt.ylabel('Northern',fontsize='large')
	plt.title('Ice Volume (10**9 m**3)',fontsize='large')

	plt.subplot(nbzone,nbplotline,2)
	plt.plot(year, NArea, 'k')
	plt.grid(True)
	plt.axis([min(year), max(year), 0, 15000])
	plt.title('Ice Area (10**9 m**2)',fontsize='large')

	plt.subplot(nbzone,nbplotline,3)
	plt.plot(year, NExtent, 'k')
	plt.grid(True)
	plt.axis([min(year), max(year), 0, 20000])
	plt.title('Ice extent (10**9 m**2)',fontsize='large')

if south == 1 :  
	plt.subplot(nbzone,nbplotline,north*nbplotline + 1)
	plt.plot(year, SVolume, 'k')
	plt.grid(True)
	plt.axis([min(year), max(year), 0, 20000])
	plt.ylabel('Southern',fontsize='large')
	plt.title('Ice Volume (10**9 m**3)',fontsize='large')

	plt.subplot(nbzone,nbplotline,north*nbplotline + 2)
	plt.plot(year, SArea, 'k')
	plt.grid(True)
	plt.axis([min(year), max(year), 0, 20000])
	plt.title('Ice Area (10**9 m**2)',fontsize='large')

	plt.subplot(nbzone,nbplotline,north*nbplotline + 3)
	plt.plot(year, SExtent, 'k')
	plt.grid(True)
	plt.axis([min(year), max(year), 0, 20000])
	plt.title('Ice extent (10**9 m**2)',fontsize='large')

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_icemonth.png')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 
