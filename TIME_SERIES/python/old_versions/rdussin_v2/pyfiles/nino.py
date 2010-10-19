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

listfiles   = ['NINO12','NINO3','NINO4','NINO34']
datalist    = ['nino12','nino3','nino4','nino34']
dataobslist = ['YEAR_ELNINO','MONTH_ELNINO','NINO1+2','NINO3','NINO4','NINO3.4',
               'YEAR_SOI','MONTH_SOI','SOI']
dataobs     = ['YEAR_ELNINO','MONTH_ELNINO','nino12obs','nino3obs','nino4obs','nino34obs',
               'YEAR_SOI','MONTH_SOI','SOI']

fig_size =  [15.,18.]

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
### Read Netcdf
###############################################################################################

## model
for k in range(len(listfiles)) :
	fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles[k] + '.nc'
	vars()[datalist[k]] = mf.read_timeserie1d(fich, 'mean_3Dvotemper')

fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles[0] + '.nc'
year = mf.get_time_counter(fich, frstyear)

## obs
for k in range(len(dataobslist)) :
	vars()[dataobs[k]] = mf.read_dimension1d(fich_data, dataobslist[k])

yearobs = YEAR_ELNINO + (MONTH_ELNINO - 1) / 12

datSOI = YEAR_SOI + (MONTH_SOI - 1) / 12


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
plt.plot(year, nino12, 'k', yearobs, nino12obs, 'g')
plt.axis([min(year), max(max(year),max(yearobs)), min(min(nino12),min(nino12obs)), max(max(nino12),max(nino12obs))])
plt.grid(True)
plt.ylabel('Nino1+2', fontsize='small')

plt.subplot(5,1,2)
plt.plot(year, nino3, 'k', yearobs, nino3obs, 'g')
plt.axis([min(year), max(max(year),max(yearobs)), min(min(nino3),min(nino3obs)), max(max(nino3),max(nino3obs))])
plt.grid(True)
plt.ylabel('Nino3', fontsize='small')

plt.subplot(5,1,3)
plt.plot(year, nino4, 'k', yearobs, nino4obs, 'g')
plt.axis([min(year), max(max(year),max(yearobs)), min(min(nino4),min(nino4obs)), max(max(nino4),max(nino4obs))])
plt.grid(True)
plt.ylabel('Nino4', fontsize='small')

plt.subplot(5,1,4)
plt.plot(year, nino34, 'k', yearobs, nino34obs, 'g')
plt.axis([min(year), max(max(year),max(yearobs)), min(min(nino34),min(nino34obs)), max(max(nino34),max(nino34obs))])
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

plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_nino.png')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 
