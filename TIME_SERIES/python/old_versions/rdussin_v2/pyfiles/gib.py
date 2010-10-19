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

filet    = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_TGIB.nc'
files    = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_SGIB.nc'
filetlev = datadir + '/' + 'LEVITUS_y0000_TGIB.nc'
fileslev = datadir + '/' + 'LEVITUS_y0000_SGIB.nc'

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

fig_size  = [15.,18.]
levelanom = 25    # level where we compute the anomaly
maxdepth  = 5000. # for plots
smin      = 34.5  # for plots

levels = mf.read_dimension1d(filet,    'gdept')
year   = mf.get_time_counter(filet,    frstyear)

tlev   = mf.read_dimension1d(filetlev, 'mean_votemper')
slev   = mf.read_dimension1d(fileslev, 'mean_vosaline')
tmodel = mf.read_timeserie2d(filet,    'mean_votemper' , len(year), len(levels))
smodel = mf.read_timeserie2d(files,    'mean_vosaline' , len(year), len(levels))

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "gib.py : use default values (for global), 46 levels"
elif config.find('NATL12') ==0 :
	print "gib.py : use values for NATL12, 64 levels"
	levelanom=37
elif config.find('NATL025') == 0 :
	print "gib.py : use default values (suitable for NATL025)"
else :
	print "gib.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration, 46 levels"

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

tmodel = mf.remove_spval(tmodel, 999. , 0.)
smodel = mf.remove_spval(smodel, 999. , 0.)
tlev   = mf.remove_spval(tlev  , 999. , 0.)
slev   = mf.remove_spval(slev  , 999. , 0.)

tlev2D = ones((size(year),1,)) * tlev
slev2D = ones((size(year),1,)) * slev

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

ncontour=75

###############################################################################################
### PLOTS part 2 : general script for plotting
###############################################################################################

plt.subplot(3,2,1)
plt.plot(tlev, levels, 'b', tmodel[-1,:], levels, 'r')
plt.grid(True)
plt.axis([min(min(tmodel[-1,:]),min(tlev)), max(max(tmodel[-1,:]),max(tlev)),maxdepth,min(levels)])
plt.ylabel('Depth')
plt.title('Last Tgib profile 14W-10W, 36N-40N (blue is Levitus)')

plt.subplot(3,2,2)
plt.contourf(year, levels, transpose(tmodel-tlev2D),ncontour)
plt.colorbar()
plt.grid(True)
plt.axis([min(year), max(year),maxdepth,min(levels)])
plt.ylabel('Temperature anomaly')

plt.subplot(3,2,3)
plt.plot(slev, levels, 'b', smodel[-1,:], levels, 'r')
plt.grid(True)
plt.axis([smin, max(max(smodel[-1,:]),max(slev)),maxdepth,min(levels)])
plt.ylabel('Depth')
plt.title('Last Sgib profile 14W-10W, 36N-40N (blue is Levitus)')

plt.subplot(3,2,4)
plt.contourf(year, levels, transpose(smodel-slev2D),ncontour)
plt.colorbar()
plt.grid(True)
plt.axis([min(year), max(year),maxdepth,min(levels)])
plt.ylabel('Salinity anomaly')

lvlm1 = levelanom -1 # for python indexing of arrays

plt.subplot(3,2,5)
plt.plot(year, (tmodel[:,lvlm1]-tlev[lvlm1]), 'r.-')
plt.grid(True)
plt.axis([min(year), max(year), min(tmodel[:,lvlm1]-tlev[lvlm1]), max(tmodel[:,lvlm1]-tlev[lvlm1])])
plt.ylabel('T anom (1000m)')
plt.title('Temperature Anomaly Level ' + str(levelanom))

plt.subplot(3,2,6)
plt.plot(year, (smodel[:,lvlm1]-slev[lvlm1]), 'r.-')
plt.grid(True)
plt.axis([min(year), max(year), min(smodel[:,lvlm1]-slev[lvlm1]), max(smodel[:,lvlm1]-slev[lvlm1])])
plt.ylabel('S anom (1000m)')
plt.title('Salinity Anomaly Level ' + str(levelanom))

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_gib.png')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) Timeseries a la fois en 1d et 2d : dur de compacter le code. Le risque est de le rendre
#    vraiment illisible 
# 
#
