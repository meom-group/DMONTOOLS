#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""cable : monitoring of transports for drakkar runs
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

plot_name = 'cable'
fig_size =  [5.,5.]
plt.rcParams.update({'figure.figsize': fig_size})

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
          dummy, file_obs = _get_ncname(argdict=argdict)
          return _readnc(fromfile[0], file_obs, argdict=argdict) 
       elif fromfile[0].endswith('.mtl'): # if mtlfile name is provided
          print 'mtl files are no longer supported'
       else:                               
          pass
    elif fromfile==[]:                    # production mode 
       file_nc, file_obs   = _get_ncname(argdict=argdict)
       # try to open a netcdf file
       if os.path.isfile(file_nc) and os.path.isfile(file_obs):
          return _readnc(file_nc, file_obs, argdict=argdict) 
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_TRANSPORTS.nc' 
    fileobs  = argdict['dataobsdir'] + osp + 'data_obs_DRAKKAR.nc'
    return filename, fileobs

#=======================================================================

def _readnc(filenc=None,fileobs=None,argdict=myargs):
    # get the section names corresponding to the config
    (truenames, shortnames, longnames, sens) = rs.define_sections(argdict)
    # test on existence of florida bahamas section in list
    found = None
    for k in range(len(truenames)):
        if truenames[k].find('FLORIDA_BAHAMAS') >= 0:
            found = 1
    if found is None:
        print "Your domain do not contain the Florida Bahamas section"
        sys.exit()    
    else:
        pass
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['datemodel'] = rs.get_datetime(filenc)
    outdict['trpmodel']  = -1 * rs.readfilenc(filenc, 'vtrp_floba' )
    outdict['dateobs']   = rs.get_datetime(fileobs,tname='YEAR_CABLE')
    outdict['trpobs']    = rs.readfilenc(fileobs, 'CABLE')

    return outdict # return the dictionnary of values 


#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None,color='r',compare=False,datemodel=None,trpmodel=None,dateobs=None,trpobs=None):
    #
    if figure is None :
        figure = plt.figure()
    ax = figure.add_subplot(111)
    ax.plot(datemodel,trpmodel,color + '.-',dateobs,trpobs,'b.-')
    _datemodel = ps.mdates.date2num(datemodel) # now a numerical value
    _dateobs = ps.mdates.date2num(dateobs)     # idem
    ax.axis([min(_datemodel),max(max(_datemodel),max(_dateobs)),min(min(trpmodel),min(trpobs)), max(max(trpmodel),max(trpobs))])
    ax.grid(True)
    ps.set_dateticks(ax)
    figure.autofmt_xdate()
    if not(compare) :
         plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Mass Transport - Obs (b)',fontsize='small')
    else:
         plt.title('Mass Transport - Obs (b)',fontsize='small')
    plt.ylabel('Transport cross cable section',fontsize='small')
    #
    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_cable.png')

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
      fig.savefig('./cable.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

