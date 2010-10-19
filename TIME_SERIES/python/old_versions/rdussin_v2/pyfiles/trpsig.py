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

listfiles = ['01_Denmark_strait' , '02_Faoes_Bank_Channel', '03_Gibraltar' , '04_Gibbs_FZ' ,
             '05_Romanche_FZ' , '06_Vema_Channel' , '07_Bab_el_Mandeb' , '08_Lombok' ,
             '09_Mona' , '10_Windward_passage']

datalist =  ['Denmark_strait' , 'Faoes_Bank_Channel', 'Gibraltar' , 'Gibbs_FZ' ,
             'Romanche_FZ' , 'Vema_Channel' , 'Bab_el_Mandeb' , 'Lombok' ,
             'Mona' , 'Windward_passage']

titleplot =  ['Denmark_strait' , 'Faoes_Bank_Channel', 'Gibraltar' , 'Gibbs_FZ' ,
             'Romanche_FZ' , 'Vema_Channel' , 'Bab_el_Mandeb' , 'Lombok' ,
             'Mona' , 'Windward_passage']

fig_size = [10.,10.]
ncontour = 75

params = {'axes.labelsize': 12,
          'text.fontsize': 12,
          'legend.fontsize': 12,
          'xtick.labelsize': 12,
          'ytick.labelsize': 12,
          'figure.figsize': fig_size}
plt.rcParams.update(params)

###############################################################################################
### AUTOMATIC CONFIG SELECTOR
### -> update default values if necessary to match the user's config
###############################################################################################

if config.find('ORCA') == 0 :
	print "trpsig.py : use default values (for global)"
elif config.find('NATL') == 0:
	print "trpsig.py : use custom values for NATL configs"
	listfiles = ['01_Denmark_strait' , '02_Faoes_Bank_Channel', '03_Gibraltar' , '04_Gibbs_FZ' ,
	'05_Romanche_FZ']
	datalist =  ['Denmark_strait' , 'Faoes_Bank_Channel', 'Gibraltar' , 'Gibbs_FZ' ,
	'Romanche_FZ']
	titleplot =  ['Denmark_strait' , 'Faoes_Bank_Channel', 'Gibraltar' , 'Gibbs_FZ' ,
	'Romanche_FZ']

elif config.find('PERIANT') == 0 :
        titleplot=['Drake Passage','South Australia']
        print "trpsig.py : use custom values for PERIANT configs"
else :
	print "trpsig.py : Your config is not supported..."
	print "The monitoring will try to use default values from Global Configuration"

###############################################################################################
### Read netcdf and plot in separate files
###############################################################################################

fichtable = open(datadir + '/' + 'drakkar_trpsig_table.txt','r')
sections=[sections for sections in fichtable.readlines() if sections.find('#') != 0 ] # remove empty lines
fichtable.close()

fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles[0] + '_trpsig.nc'
year  = mf.get_time_counter(fich, frstyear)
sigma = mf.read_timeserie2d(fich, 'sigma_class', len(year), -1)[0,:]


for k in range(len(listfiles)) :
	fich = datadir + '/' + config + '-' + case + '_y' + frstyear + '-' + lastyear + '_' + listfiles[k] + '_trpsig.nc'
        lines=[lines for lines in sections if lines.find(listfiles[k]) == 0 ]
        sec=lines[0].split()[1]
	vars()[datalist[k]] = mf.read_timeserie2d(fich, 'sigtrp' + '_' + sec, len(year) ,len(sigma)) / 1e6
	#vars()[datalist[k]] = mf.add_spval(vars()[datalist[k]], 0.)
	nameplot = plotdir_confcase + '/' + config + '-' + case + '_' + listfiles[k] + '_trpsig.png'
	mf.plot_trpsig(year, sigma, vars()[datalist[k]], ncontour, titleplot[k], nameplot)


###############################################################################################
### DEVELOPPERS NOTES
###############################################################################################
#
# 1) sections for NATL configs may have to be corrected (dens_section.dat weird)
# 
# 
