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

datalist_march    = ['NVolume', 'NArea','NExtent']
datalist_sept     = ['SVolume', 'SArea','SExtent']
dataobslist_north = ['YEAR_ICE_NORTH','MONTH_ICE_NORTH','NORTH_ICE_EXTENT','NORTH_ICE_AREA']
dataobslist_south = ['YEAR_ICE_SOUTH','MONTH_ICE_SOUTH','SOUTH_ICE_EXTENT','SOUTH_ICE_AREA']

fig_size =  [15.,15.]
nbplot   = 6

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "icetrd.py : use default values (for global)"
elif config.find('PERIANT') == 0 :
        north = 0
	south = 1
        fig_size =  [15.,6.]
	nbplot = 3
        print "icetrd.py : use custom values for PERIANT configs"
else :
	print "icetrd.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### Read the Netcdf files
###############################################################################################

if north == 1 :
	fich_march = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + 'm03_ice.nc'
	year = mf.get_time_counter(fich_march, frstyear)
	for k in range(len(datalist_march)) :
		vars()[datalist_march[k]] = mf.read_timeserie1d(fich_march, datalist_march[k])
	for k in range(len(dataobslist_north)) :
		vars()[dataobslist_north[k]] = mf.read_dimension1d(fich_data, dataobslist_north[k])
	yearobs_north    = mf.extract_month(YEAR_ICE_NORTH  , MONTH_ICE_NORTH, 3)
	extentobs_north  = mf.extract_month(NORTH_ICE_EXTENT, MONTH_ICE_NORTH, 3)
	areaobs_north    = mf.extract_month(NORTH_ICE_AREA  , MONTH_ICE_NORTH, 3)
	extentobs_north  = mf.remove_nan(extentobs_north)
	areaobs_north    = mf.remove_nan(areaobs_north)
	# Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after 
	# ( Change of satellite SSM/R SSM/I )
	indN = mf.getMyLatIndex(yearobs_north, 1987)
	for k in range(indN+1) :
		areaobs_north[k] = areaobs_north[k] + 1.19
	for k in range(indN+1,len(areaobs_north)) :
		areaobs_north[k] = areaobs_north[k] + 0.31

if south == 1 :
	fich_sept = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + 'm09_ice.nc'
	year = mf.get_time_counter(fich_sept, frstyear)
	for k in range(len(datalist_sept)) :
		vars()[datalist_sept[k]] = mf.read_timeserie1d(fich_sept, datalist_sept[k])
	for k in range(len(dataobslist_south)) :
		vars()[dataobslist_south[k]] = mf.read_dimension1d(fich_data, dataobslist_south[k])
	yearobs_south    = mf.extract_month(YEAR_ICE_SOUTH  , MONTH_ICE_SOUTH, 3)
	extentobs_south  = mf.extract_month(SOUTH_ICE_EXTENT, MONTH_ICE_SOUTH, 9)
	areaobs_south    = mf.extract_month(SOUTH_ICE_AREA  , MONTH_ICE_SOUTH, 9)
	extentobs_south  = mf.remove_nan(extentobs_south)
	areaobs_south    = mf.remove_nan(areaobs_south)


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

nbzone = north + south
nbplotline = nbplot/nbzone # warning, we work with integer

###############################################################################################
### PLOTS part 2 : general script for plotting
###############################################################################################

if north == 1 :
	plt.subplot(nbzone,nbplotline,1)
	plt.plot(year, NVolume/1000, 'k.-')
	#plt.axis([1950, 2010, 0, 20])
	plt.grid(True)
	plt.title('Volume Arctic March',fontsize='large')

	plt.subplot(nbzone,nbplotline,2)
	plt.plot(year, NArea/1000, 'k.-',yearobs_north, areaobs_north, 'b.-')
	plt.grid(True)
	#plt.axis([1950, 2010, 0, 15])
	plt.title('Area Arctic March',fontsize='large')

	plt.subplot(nbzone,nbplotline,3)
	plt.plot(year, NExtent/1000, 'k.-',yearobs_north, extentobs_north, 'b.-')
	plt.grid(True)
	#plt.axis([1950, 2010, 0, 30])
	plt.title('Extent Arctic March',fontsize='large')

if south == 1 :
	plt.subplot(nbzone,nbplotline,north*nbplotline + 1)
	plt.plot(year, SVolume/1000, 'k.-')
	plt.grid(True)
	#plt.axis([1950, 2010, 0, 20])
	plt.title('Volume Antarctic September',fontsize='large')

	plt.subplot(nbzone,nbplotline,north*nbplotline + 2)
	plt.plot(year, SArea/1000, 'k.-', yearobs_south, areaobs_south, 'b.-')
	plt.grid(True)
	#plt.axis([1950, 2010, 0, 30])
	plt.title('Area Antarctic September',fontsize='large')
	
	plt.subplot(nbzone,nbplotline,north*nbplotline + 3)
	plt.plot(year, SExtent/1000, 'k.-', yearobs_south, extentobs_south, 'b.-')
	plt.grid(True)
	#plt.axis([1950, 2010, 0, 20])
	plt.title('Extent Antarctic September',fontsize='large')

### SAVE THE FIGURE ###
plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_icetrd.png')


### notes : comparaison avec fig matlab -> doute, peut etre un bug qq part...

