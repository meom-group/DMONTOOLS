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

listfiles_max = ['Glo_maxmoc40N' , 'Atl_maxmoc40N', 'Glo_maxmoc30S', 'Atl_maxmoc30S', 'Aus_maxmoc50S']
listfiles_min = ['Inp_minmoc30S']
datalist_max  = ['mocglo40' , 'mocatl40', 'mocglo30', 'mocatl30', 'mocaus50']
datalist_min  = ['mocinp30']

titleplot     = ['MOC at 40N', 'MOC at 30S', 'MOC at 40N',  'MOC at 30S','MOC at 30S','MOC at 50S']
listylabel    = ['Global', 'Atlantic', 'Indo-Pacific']
nbzone        =  3
nbfig         =  2
fig_size      = [15.,20.]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "maxmoc40.py : use default values (for global)"
elif config.find('NATL') == 0 :
	listfiles_max = ['Glo_maxmoc40N' , 'Glo_maxmoc15S']
	listfiles_min = []		
	datalist_max  = ['mocglo40', 'mocglo15']
	datalist_min  = []
	titleplot     = ['MOC at 40N', 'MOC at 30S']
	listylabel    = ['Atlantic']
	nbzone        =  1
	nbfig         =  2
	fig_size      = [10.,6.]

	print "maxmoc40.py : use custom values for NATL configs"
else :
	print "maxmoc40.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### Read NetCDF files
###############################################################################################

for k in range(len(listfiles_max)) :
	fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles_max[k] + '.nc'
	vars()[datalist_max[k]] = mf.read_timeserie1d(fich, 'maxmoc')

for k in range(len(listfiles_min)) :
	fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles_min[k] + '.nc'
	vars()[datalist_min[k]] = mf.read_timeserie1d(fich, 'minmoc')

fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles_max[0] + '.nc'
year = mf.get_time_counter(fich, frstyear)

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

# In which order we want to plot
if config.find('ORCA') == 0 :
	datalist = ['mocglo40' , 'mocglo30' , 'mocatl40', 'mocatl30', 'mocinp30' , 'mocaus50' ]
else :
	datalist = datalist_max + datalist_min

###############################################################################################
### PLOTS part 2 : general script for plotting
###############################################################################################

for k in range(len(datalist)) :
	plt.subplot(nbzone, nbfig, k+1)
	plt.plot(year,vars()[datalist[k]],'r.-')
	plt.axis([min(year), max(year), 
	min(vars()[datalist[k]]), max(vars()[datalist[k]])])
	plt.grid(True)
	plt.title(titleplot[k],fontsize='small')
	if divmod(k,nbfig)[1] == 1 :
		plt.ylabel(listylabel[divmod(k,nbfig)[0]],fontsize='small')

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_maxmoc40.png')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 


