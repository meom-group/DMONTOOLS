import matplotlib.pylab as plt
import Scientific.IO.NetCDF
import os

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

filet    = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_TMEAN.nc'
files    = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_SMEAN.nc'
filessh  = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_SSHMEAN.nc'
filetlev = datadir + '/' + 'LEVITUS_y0000_TMEAN.nc'
fileslev = datadir + '/' + 'LEVITUS_y0000_SMEAN.nc'

###############################################################################################
### DEFAULT VALUES SUITABLE FOR GLOBAL
###############################################################################################

fig_size =  [15.,18.]

tmean   = mf.read_timeserie1d(filet,   'mean_3Dvotemper')
smean   = mf.read_timeserie1d(files,   'mean_3Dvosaline')
sshmean = mf.read_timeserie1d(filessh, 'mean_3Dsossheig')
levels  = mf.read_dimension1d(filet,   'gdept')
year    = mf.get_time_counter(filet, frstyear)

tmodel = mf.read_timeserie2d(filet,'mean_votemper', len(year), len(levels))
smodel = mf.read_timeserie2d(files,'mean_vosaline', len(year), len(levels))

tlev    = mf.read_dimension1d(filetlev, 'mean_votemper')
slev    = mf.read_dimension1d(fileslev, 'mean_vosaline')

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "tsmean_lev.py : use default values (for global)"
elif config.find('NATL') == 0 :
	print "tsmean_lev.py : use default values (suitable for NATL)"
elif config.find('PERIANT') == 0 :
        print "tsmean_lev.py : use default values (suitable for PERIANT)"
else :
	print "tsmean_lev.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

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

### 1) PLOTS OF 3D MEANS
list3Dmean = ['tmean', 'smean', 'sshmean']
titles3Dmean = ['3D Global T Mean', '3D Global S Mean', '3D Global SSH Mean']
nb3Dmean=len(list3Dmean)

for k in range(nb3Dmean) :
	plt.subplot(3,nb3Dmean,k+1)
	plt.plot(year, vars()[list3Dmean[k]], 'r.-')
	plt.grid(True)
	plt.axis([min(year), max(year),min(vars()[list3Dmean[k]]),max(vars()[list3Dmean[k]])])
	plt.title(titles3Dmean[k])

### 2) 2D PLOTS
ncontour=75

plt.subplot(3,nb3Dmean,nb3Dmean+1)
plt.semilogy((tmodel[-1,:]-tlev[:]),levels,'r')
plt.grid(True)
plt.axis([min(tmodel[-1,:]-tlev[:]), max(tmodel[-1,:]-tlev[:]),max(levels),min(levels)])
plt.ylabel('Log Depth')
plt.title('Global change in T')

plt.subplot(3,nb3Dmean,nb3Dmean+2)
plt.contourf(year, levels, transpose(tmodel[:,:]-tlev[:]),ncontour)
plt.colorbar()
plt.grid(True)
plt.yscale('log')
plt.axis([ min(year), max(year), max(levels),min(levels) ])
plt.ylabel('Temperature anomaly')

plt.subplot(3,nb3Dmean,2*nb3Dmean+1)
plt.semilogy((smodel[-1,:]-slev[:]),levels,'r')
plt.grid(True)
plt.axis([min(smodel[-1,:]-slev[:]), max(smodel[-1,:]-slev[:]),max(levels),min(levels)])
plt.ylabel('Log Depth')
plt.title('Global change in S')

plt.subplot(3,nb3Dmean,2*nb3Dmean+2)
plt.contourf(year, levels, transpose(smodel[:,:]-slev[:]),ncontour)
plt.colorbar()
plt.grid(True)
plt.yscale('log')
plt.axis([ min(year), max(year), max(levels),min(levels) ])
plt.ylabel('Salinity anomaly')

###############################################################################################
### SAVE THE FIGURE
###############################################################################################

plt.savefig(plotdir_confcase + '/' + config + '-' + case + '_tsmean_lev.png')

###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) This should be ok for every config.
# 2) organisation generale du code a revoir (notament place config selector)  
#
#
