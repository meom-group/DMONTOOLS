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

fich_model = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_07_FLORIDA_BAHAMAS.nc'
fig_size =  [5.,5.]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "cable.py : use default values (for global)"
elif config.find('NATL') == 0 :
	print "cable.py : use custom values for NATL configs"
elif config.find('PERIANT') == 0 :
        print "cable.py : use custom values for PERIANT configs"
else :
	print " cable.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### Read NetCDF files
###############################################################################################

year = mf.get_time_counter(fich_model, frstyear)
masstrans_model = -1 * mf.read_timeserie1d(fich_model, 'vtrp')

yearobs = mf.read_dimension1d(fich_data, 'YEAR_CABLE')
masstrans_obs = mf.read_dimension1d(fich_data, 'CABLE')


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

plt.plot(year,masstrans_model,'r.-',yearobs,masstrans_obs,'b.-')
plt.axis([min(min(year),min(yearobs)) , max(max(year),max(yearobs)), 
          min(min(masstrans_model),min(masstrans_obs)), 
          max(max(masstrans_model),max(masstrans_obs))])
plt.grid(True)
plt.title('Mass Transport',fontsize='small')
plt.ylabel('Florida Bahamas' ,fontsize='small')


###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_cable.png')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
#
