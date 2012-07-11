#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""maxmoc : monitoring of MOC and MHT for drakkar runs
"""
#-----------------------------------------------------------------------
#                       Additional Documentation
#
# Modification History:
#  - May 2010: Original module by R. Dussin and J. Le Sommer
#
# Copyright (c) 2010, R. Dussin and J. Le Sommer. For licensing, distribution
# conditions, contact information, and additional documentation see the wiki :
# https://servforge.legi.grenoble-inp.fr/projects/DMONTOOLS/wiki/TIME_SERIES/python.
#=======================================================================

#-----------------------------------------------------------------------

#- Module General Import and Declarations

import sys,os
import numpy as npy
import pydmontools 
from pydmontools import __readtools__ as rs
from pydmontools import __plottools__ as ps
plt = ps.plt 

myargs = pydmontools.default_argdict
osp = os.sep

#- parameters

plot_name = 'maxmoc'

#=======================================================================
#--- Reading the data 
#=======================================================================

def read(argdict=myargs,fromfile=[]):
    #
    if fromfile!=[]:                      # diagnostic mode...
       if fromfile[0].endswith('.nc'):    # if ncfile name is provided
          if not(len(fromfile)==2):
             print 'please provide two netcdf filename : MOC and MHT'
             sys.exit() 
          return _readnc(fromfile[0], fromfile[1], argdict=argdict) 
       elif fromfile[0].endswith('.mtl'): # if mtlfile name is provided
          print 'mtl files are no longer supported'
	  sys.exit()
       else:                               
          pass
    elif fromfile==[]:                    # production mode 
       file_nc, file_nc2  = _get_ncname(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(file_nc) and os.path.isfile(file_nc2):
          return _readnc(file_nc, file_nc2, argdict=argdict) 
       else:
          print '>>> maxmoc.py : No files found for frequency : ' + argdict['monitor_frequency'] ; exit()

          
def _get_ncname(argdict=myargs):
    #
    if rs.check_freq_arg(argdict['monitor_frequency']):

        filename = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_MAXMOC.nc'

        filename2 = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_MHT.nc'

    return filename,filename2

#=======================================================================

def _readnc(filenc=None, filenc2=None, argdict=myargs):
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['date_model']      = rs.get_datetime(filenc)
    outdict['date_model_heat'] = rs.get_datetime(filenc2)
    if argdict['config'].find('ORCA') == 0:
        data_list = ['maxmoc_Glo_maxmoc' , 'maxmoc_Atl_maxmoc', 'maxmoc_Aus_maxmoc',
                     'minmoc_Glo_minmoc' , 'minmoc_Atl_minmoc', 'minmoc_Inp_minmoc',
                     'minmoc_Inp_minmoc2', 'minmoc_Aus_minmoc'] 
        data_list_heat = ['zomht_glo', 'zomht_atl']
    elif argdict['config'].find('NATL') == 0:
        data_list = ['maxmoc_Glo_maxmoc', 'minmoc_Glo_minmoc' ]
        data_list_heat = ['zomht_glo']
    elif 'PERIANT' in argdict['config']: # this should be checked. 
	data_list = ['maxmoc_Glo_maxmoc', 'minmoc_Glo_minmoc' ]
	data_list_heat = []
    else:
        print "config not supported"
        sys.exit()
    #
    for k in data_list:
        outdict[k] = rs.readfilenc(filenc,k)
    #
    lat = rs.readfilenc(filenc2,'nav_lat')
    index20N  = rs.get_index(lat, 20.)
    for k in data_list_heat:
        outdict[k] = rs.readfilenc(filenc2,k)[:,index20N]
 
    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, date_model=None, date_model_heat=None,figure=None, color='r', compare=False, **kwargs):
    #
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #
    _date_model = ps.mdates.date2num(date_model) # now a numerical value
    _date_model_heat = ps.mdates.date2num(date_model_heat) # now a numerical value

    if argdict['config'].find('ORCA') == 0:
        #
        data_list_plt = ['maxmoc_Glo_maxmoc', 'minmoc_Glo_minmoc' , 'zomht_glo',
                         'maxmoc_Atl_maxmoc', 'minmoc_Atl_minmoc' , 'zomht_atl',
                         'minmoc_Inp_minmoc', 'minmoc_Inp_minmoc2', 'unused',
                         'maxmoc_Aus_maxmoc', 'minmoc_Aus_minmoc', 'unused'] 

        titleplot= ['Max Overturning (20N-60N 500m-2000m)', 'Min Overturning (-40S 30N 2000m-5500m)',
            'MHT a 20N', 'Max Overturning (0N-60N 500m-2000m)', 
            'Min Overturning (-20S 40N 2000m-5500m)', 'MHT at 20N', 'Min Overturning (15N 50N 100m-1000m)',
            'Min Overturning (-30S 20N 1000m-5500m)', '',
            'Max Overturning (-70S 0S 0m-2000m)',  'Min Overturning (-70S 0S 2000m-5500m)' ,'']

        listylabel=['Global', 'Atlantic', 'Indo-Pacific', 'Austral'] ; nbzone=4 ; nbfig=3
        #
    elif argdict['config'].find('NATL') == 0:
        data_list_plt = ['maxmoc_Glo_maxmoc', 'minmoc_Glo_minmoc','zomht_glo']
        titleplot = ['Max Overturning (0N-60N 500m-2000m)', 'Min Overturning (-20S 40N 2000m-5500m)', 'MHT at 20N']
        listylabel=['Atlantic'] 
	nbzone=1
	nbfig=3
    elif argdict['config'].find('PERIANT') == 0:
        data_list_plt = ['maxmoc_Glo_maxmoc', 'minmoc_Glo_minmoc']
        titleplot = ['Max Overturning (-70S 0S 0m-2000m)',  'Min Overturning (-70S 0S 2000m-5500m)']
        listylabel=['Austral'] 
	nbzone=1
	nbfig=2
    else:
        print "config not supported"
        sys.exit()
    #
    fig_size=[float(nbfig) * 6., float(nbzone) * 5.]
    if figure is None: # by default create a new figure
        figure = plt.figure(figsize=fig_size)
    #
    for k in range(len(data_list_plt)) :
        if data_list_plt[k].find('unused') != 0 :
            ax = figure.add_subplot(nbzone, nbfig, k+1)
            ax.plot(date_model,vars()[data_list_plt[k]],color + '.-')
            ax.axis([min(_date_model), max(_date_model), 
            min(vars()[data_list_plt[k]]), max(vars()[data_list_plt[k]])])
            plt.grid(True)
	    ps.set_dateticks(ax)
            if not(compare) and k <= 2 :
                plt.title(argdict['config'] + '-' + argdict['case']+'\n'+titleplot[k],fontsize='small')
            else :
                plt.title(titleplot[k],fontsize='small')
            if divmod(k,nbfig)[1] == 1 :
                plt.ylabel(listylabel[divmod(k,nbfig)[0]],fontsize='small')
    #figure.autofmt_xdate() # should be adapted a bit more...
    #
    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    monit_freq = argdict['monitor_frequency']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_' + monit_freq + '_maxmoc.png')

#=======================================================================
#--- main 
#=======================================================================

def main(): 
   # get arguments
   parser = pydmontools.standard_pydmt_script_parser()
   (options, args) = parser.parse_args()
   argdict = myargs
   argdict.update(vars(options))
   infiles = args # default value is an empty list
   # read, plot and save   
   values = read(argdict=argdict,fromfile=infiles)
   fig = plot(argdict=argdict,**values)
   if len(args)==0:
      save(argdict=argdict,figure=fig)
   else:
      fig.savefig('./maxmoc.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

