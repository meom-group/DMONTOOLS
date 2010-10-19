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

fileheattrp = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_heattrp.nc'
listfiles_max = ['Glo_maxmoc' , 'Atl_maxmoc', 'Aus_maxmoc']
listfiles_min = ['Glo_minmoc' , 'Atl_minmoc', 'Inp_minmoc' , 'Inp_minmoc2', 'Aus_minmoc']
datalist_max  = ['mocglomax', 'mocatlmax', 'mocausmax']
datalist_min  = ['mocglomin', 'mocatlmin', 'mocipn', 'mocips',  'mocausmin']
datalist_mht  = ['zomht_glo', 'zomht_atl']

listplot      = ['mocglomax', 'mocglomin', 'zomht_glo', 'mocatlmax', 'mocatlmin', 'zomht_atl',
                 'mocipn', 'mocips', 'unused', 'mocausmax', 'mocausmin', 'unused']

titleplot= ['Max Overturning (20N-60N 500m-2000m)', 'Min Overturning (-40S 30N 2000m-5500m)',
            'MHT a 20N', 'Max Overturning (0N-60N 500m-2000m)', 
            'Min Overturning (-20S 40N 2000m-5500m)', 'MHT at 20N', 'Min Overturning (15N 50N 100m-1000m)',
            'Min Overturning (-30S 20N 1000m-5500m)', '',
            'Max Overturning (-70S 0S 0m-2000m)',  'Min Overturning (-70S 0S 2000m-5500m)' ,'']

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
	listfiles_max = ['Glo_maxmoc' ]
	listfiles_min = ['Glo_minmoc' ]
	datalist_max  = ['mocglomax']
	datalist_min  = ['mocglomin']
	datalist_mht  = ['zomht_glo']
	listplot      = ['mocglomax','mocglomin','zomht_glo']
	titleplot = ['Max Overturning (0N-60N 500m-2000m)', 'Min Overturning (-20S 40N 2000m-5500m)', 'MHT at 20N']
	nbzone=1
	nbfig=3
	fig_size =  [15.,6.]
	print "maxmoc.py : use custom values for NATL configs"
else :
	print "maxmoc.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### GLOBAL READING OF THE NC FILES
###############################################################################################

year      = mf.get_time_counter(fileheattrp, frstyear)
lat       = mf.read_dimension1d(fileheattrp, 'nav_lat')
index20N  = mf.getMyLatIndex(lat, 20.)

for k in range(len(listfiles_max)) :
	fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles_max[k] + '.nc'
	vars()[datalist_max[k]] = mf.read_timeserie1d(fich, 'maxmoc')
	
for k in range(len(listfiles_min)) :
	fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles_min[k] + '.nc'
	vars()[datalist_min[k]] = mf.read_timeserie1d(fich, 'minmoc')

for k in range(len(datalist_mht)) :
	vars()[datalist_mht[k]] = mf.read_timeserie2d(fileheattrp, datalist_mht[k],len(year),len(lat))[:,index20N]


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

for k in range(len(listplot)) :
	if listplot[k].find('unused') != 0 :
		plt.subplot(nbzone, nbfig, k+1)
		plt.plot(year,vars()[listplot[k]],'r.-')
		plt.axis([min(year), max(year), 
		min(vars()[listplot[k]]), max(vars()[listplot[k]])])
		plt.grid(True)
		plt.title(titleplot[k],fontsize='small')
		if divmod(k,nbfig)[1] == 1 :
			plt.ylabel(listylabel[divmod(k,nbfig)[0]],fontsize='small')

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_maxmoc.png')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 
