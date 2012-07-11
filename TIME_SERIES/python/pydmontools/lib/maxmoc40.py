#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""maxmoc40 : monitoring of MOC at 40 N for drakkar runs
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

## definir une taille standard par subplot

plot_name = 'maxmoc40'

#=======================================================================
#--- Reading the data 
#=======================================================================

def read(argdict=myargs,fromfile=[]):
    #
    if fromfile!=[]:                      # diagnostic mode...
       if fromfile[0].endswith('.nc'):    # if ncfile name is provided
          if not(len(fromfile)==1):
             print 'please provide one netcdf filename'
             sys.exit() 
          return _readnc(fromfile[0], argdict=argdict) 
       elif fromfile[0].endswith('.mtl'): # if mtlfile name is provided
          print 'mtl files are no longer supported'
          sys.exit()
       else:                               
          pass
    elif fromfile==[]:                    # production mode 
       file_nc  = _get_ncname(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(file_nc):
          return _readnc(file_nc, argdict=argdict) 
       else:
          print '>>> maxmoc40.py : No files found for frequency : ' + argdict['monitor_frequency'] ; exit()

          
def _get_ncname(argdict=myargs):
    #
    if rs.check_freq_arg(argdict['monitor_frequency']):

        filename = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_MAXMOC.nc'

    return filename

#=======================================================================

def _readnc(filenc=None, argdict=myargs):
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['date_model'] = rs.get_datetime(filenc)
    if argdict['config'].find('ORCA') == 0:
        data_list = ['maxmoc_Glo_maxmoc40N' , 'maxmoc_Glo_maxmoc30S', 'maxmoc_Atl_maxmoc40N',
                     'maxmoc_Atl_maxmoc30S', 'minmoc_Inp_minmoc30S', 'maxmoc_Aus_maxmoc50S']
    elif argdict['config'].find('NATL') == 0:
        data_list = ['maxmoc_Glo_maxmoc40N' , 'maxmoc_Glo_maxmoc15S']
    else:
        print "config not supported"
        sys.exit()
    #
    for k in data_list:
        outdict[k] = rs.readfilenc(filenc,k)
 
    return outdict # return the dictionnary of values 


#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):
    #
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #
    _date_model = ps.mdates.date2num(date_model) # now a numerical value
    #
    if argdict['config'].find('ORCA') == 0:
        data_list = ['maxmoc_Glo_maxmoc40N' , 'maxmoc_Glo_maxmoc30S', 'maxmoc_Atl_maxmoc40N',
                     'maxmoc_Atl_maxmoc30S', 'minmoc_Inp_minmoc30S', 'maxmoc_Aus_maxmoc50S']
        titleplot = ['MOC at 40N', 'MOC at 30S', 'MOC at 40N',  'MOC at 30S','MOC at 30S','MOC at 50S']
        listylabel= ['Global', 'Atlantic', 'Indo-Pacific'] ; nbzone=3 ; nbfig=2
    elif argdict['config'].find('NATL') == 0:
        data_list = ['maxmoc_Glo_maxmoc40N' , 'maxmoc_Glo_maxmoc15S']
        titleplot = ['MOC at 40N', 'MOC at 30S']
	listylabel= ['Atlantic'] ; nbzone=1 ; nbfig=2
    else:
        print "config not supported"
        sys.exit()
    #
    fig_size=[float(nbfig) * 6., float(nbzone) * 5.]
    if figure is None: # by default create a new figure
        figure = plt.figure(figsize=fig_size)
    #
    for k in range(len(data_list)) :
	ax = figure.add_subplot(nbzone, nbfig, k+1)
	ax.plot(date_model,vars()[data_list[k]], color +'.-')
	ax.axis([min(_date_model), max(_date_model), 
	min(vars()[data_list[k]]), max(vars()[data_list[k]])])
	plt.grid(True)
        ps.set_dateticks(ax)

        if not(compare) and k<=1 :
	    plt.title(argdict['config'] + '-' + argdict['case']+'\n'+titleplot[k],fontsize='small')
        else :
	    plt.title(titleplot[k],fontsize='small')
	if divmod(k,nbfig)[1] == 1 :
		plt.ylabel(listylabel[divmod(k,nbfig)[0]],fontsize='small')
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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_' + monit_freq + '_maxmoc40.png')

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
      fig.savefig('./maxmoc40.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

