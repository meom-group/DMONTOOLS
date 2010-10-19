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
### GLOBAL READING OF THE NC FILES
###############################################################################################

filehtrp   = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_heattrp.nc'
filehflx   = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_hflx.nc'

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

variable      = ['Southward Accumulated Qnet', 'Advective MHT']
zone          = ['Global ', 'Atlantic ', 'Indo-Pacific ']
names_htrp    = ['zomht_glo', 'zomht_atl', 'zomht_inp']
names_hflx    = ['hflx_glo' , 'hflx_atl' , 'hflx_indopacif']

fig_size  = [15.,18.]

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "mht1.py : use default values (for global)"
elif config.find('NATL') == 0 :
	zone       = ['Global']
	names_htrp = ['zomht_glo']
	names_hflx = ['hflx_glo']
	fig_size   = [15.,6.]
	print "mht1.py : use custom values for NATL configs"
elif config.find('PERIANT') == 0 :
        zone       = ['Global']
	names_htrp = ['zomht_glo']
	names_hflx = ['hflx_glo']
        fig_size   = [15.,6.]
        print "mht1.py : use custom values for PERIANT configs"
else :
	print "mht1.py : Your config is not supported..."
	print "The monitoring will use default values from Global Configuration"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

nbvar     = len(variable)
nbzone    = len(zone)
nbzoneplt = nbzone

year = mf.get_time_counter(filehtrp, frstyear)
lat  = mf.read_dimension1d(filehtrp, 'nav_lat')

for k in range(nbzone) :
	vars()[names_htrp[k]] = mf.read_timeserie2d(filehtrp, names_htrp[k] , len(year), len(lat))
	vars()[names_hflx[k]] = mf.read_timeserie2d(filehflx, names_hflx[k] , len(year), len(lat))


for k in range(len(names_htrp)) :
	maskzeros = Numeric.greater(vars()[names_htrp[k]], 999.)
	Numeric.putmask(vars()[names_htrp[k]], maskzeros, 0.)

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

for k in range(1,nbzoneplt+1) :
	plt.subplot(nbzoneplt,nbvar+1,3*(k-1)+1)
	plt.contourf(year, lat, transpose(vars()[names_htrp[k-1]]))
	plt.colorbar(format='%.3f')
	plt.grid(True)
	plt.axis([min(year), max(year), min(lat), max(lat)])
	plt.ylabel(zone[k-1],fontsize='x-large')
	if k==1 :
		plt.title(variable[0],fontsize='large')
	plt.subplot(nbzoneplt,nbvar+1,3*(k-1)+2)
	plt.contourf(year, lat, transpose(vars()[names_hflx[k-1]]))
	plt.colorbar(format='%.3f')
	plt.grid(True)
	plt.axis([min(year), max(year), min(lat), max(lat)])
	if k==1 :
		plt.title(variable[1],fontsize='large')
	plt.subplot(nbzoneplt,nbvar+1,3*(k-1)+3)
	plt.contourf(year, lat, transpose(vars()[names_htrp[k-1]] - vars()[names_hflx[k-1]]))
	plt.colorbar(format='%.3f')
	plt.grid(True)
	plt.axis([min(year), max(year), min(lat), max(lat)])
	if k==1 :
		plt.title('Second minus first',fontsize='large')
		
###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_MHT1.png')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Works with ORCA025 and NATL12
#
