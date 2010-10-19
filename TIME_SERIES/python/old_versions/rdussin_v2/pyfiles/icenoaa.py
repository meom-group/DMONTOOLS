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

fich_data = '/home/rdussin/DRAKKAR/DEV_MONITORING/NEWPACK_PY/DATA' + '/' + 'data_obs_DRAKKAR.nc'

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

north = 1 # weither we plot or not the artic
south = 1 # weither we plot or not the antarctic

fich     = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + 'icemonth.nc'
datalist    = ['NArea','NExtent', 'SArea','SExtent']
dataobslist = ['YEAR_ICE_NORTH','MONTH_ICE_NORTH','NORTH_ICE_EXTENT','NORTH_ICE_AREA',
               'YEAR_ICE_SOUTH','MONTH_ICE_SOUTH','SOUTH_ICE_EXTENT','SOUTH_ICE_AREA']
dataobsplt = [ 'NORTH_ICE_AREA','NORTH_ICE_EXTENT', 'SOUTH_ICE_AREA','SOUTH_ICE_EXTENT']
listylabel=['Arctic Extent', 'Arctic Area', 'Antarctic Extent', 'Antarctic Area']

nbfig=4
fig_size =  [15.,15.]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "icenoaa.py : use default values (for global)"
elif config.find('PERIANT') == 0 :
        fig_size =  [15.,15.]
        print "icenoaa.py : use custom values for PERIANT configs" 
else :
	print "icenoaa.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### Read the Netcdf
###############################################################################################

year = mf.get_time_counter(fich, frstyear)

for k in range(len(datalist)) :
	vars()[datalist[k]] = mf.read_timeserie1d(fich, datalist[k])

for k in range(len(dataobslist)) :
	vars()[dataobslist[k]] = mf.read_dimension1d(fich_data, dataobslist[k])

if north == 1 :
	yearobs = YEAR_ICE_NORTH + (MONTH_ICE_NORTH - 1) / 12
elif south == 1 :
	yearobs = YEAR_ICE_SOUTH + (MONTH_ICE_SOUTH - 1) / 12
else :
	print "if both north and south are zeros, why trying to plot ???"

# Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after ( Change of satellite SSM/R SSM/I )
if north == 1 :
	indN = mf.getMyLatIndex(yearobs, 1987.5)
	for k in range(indN+1) :
		NORTH_ICE_AREA[k] = NORTH_ICE_AREA[k] + 1.19
	for k in range(indN+1,len(NORTH_ICE_AREA)) :
		NORTH_ICE_AREA[k] = NORTH_ICE_AREA[k] + 0.31

	
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

for k in range(nbfig) :
	plt.subplot(nbfig,1,k+1)
	plt.plot(year, vars()[datalist[k]]/1000, 'k',
	yearobs, vars()[dataobsplt[k]], 'b')
	plt.grid(True)
	plt.ylabel(listylabel[k],fontsize='large')


###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_icenoaa.png')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Works with ORCA025 and probably not NATL12
# 2) bug found in monitor matlab where indN was 92 and corresponds to june 86 
# 
