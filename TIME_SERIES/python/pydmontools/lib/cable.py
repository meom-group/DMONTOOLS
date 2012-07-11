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
fig_size =  [5.,5.] # minimum fig size
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
       else:
          print '>>> cable.py : No files found for frequency : ' + argdict['monitor_frequency'] ; exit()

          
def _get_ncname(argdict=myargs):
    if rs.check_freq_arg(argdict['monitor_frequency']):

        filename = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_TRANSPORTS.nc'

    fileobs  = argdict['dataobsdir'] + osp + 'dmondata_cable_NOAA-AOML.nc'

    return filename, fileobs

#=======================================================================

def _readnc(filenc=None,fileobs=None,argdict=myargs):
    # get the section names corresponding to the config

    if argdict.has_key('compared_configs'):
       compare = True
    else:
       argdict['compared_configs'] = [ argdict['config'] ]
       compare = False
    # get the section names corresponding to the config
    section_dict = rs.define_all_sections(argdict,compare)

    truenames = section_dict.keys()
    truenames.sort()

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
    try:
       outdict['trpmodel']  = -1 * rs.readfilenc(filenc, 'vtrp_floba' )
    except:
       outdict['trpmodel']  = npy.zeros((len(outdict['datemodel'])))

    outdict['dateobs']   = rs.get_datetime(fileobs)
    outdict['trpobs']    = rs.readfilenc(fileobs, 'CABLE')
    #
    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None,color='r',compare=False,datemodel=None,trpmodel=None,dateobs=None,trpobs=None):
    # deal with figure size and boundaries...
    tmin,tmax = ps.get_tminmax(datemodel)
    ymin,ymax = ps.get_vminmax(trpmodel,trpobs,ex=0.2)
    width,height = fig_size
    asp = max(width/height,(0.3*(tmax-tmin)/365.) / height) # assume time unit in days and figure height is 5.
    figsize = [ asp * height , height ]
    # proceed with the plot
    if figure is None :
        figure = plt.figure(figsize=figsize)
    ax = figure.add_subplot(111)
    ax.plot(datemodel,trpmodel,color + '.-',dateobs,trpobs,'b.-')
    ax.axis([tmin,tmax,ymin,ymax])
    ax.grid(True)
    ps.set_dateticks(ax,aspect_ratio=asp)
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
    monit_freq = argdict['monitor_frequency']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_' + monit_freq + '_cable.png')

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

