#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""icenoaa : monitoring of ice volume, area and extent compared with NOAA data for drakkar runs
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

plot_name = 'icenoaa'
fig_size =  [15.,15.]
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
	  sys.exit()
       else:                               
          pass
    elif fromfile==[]:                    # production mode 
       file_nc, file_obs  = _get_ncname(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(file_nc) and os.path.isfile(file_obs):
          return _readnc(file_nc, file_obs, argdict=argdict) 
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_ICEMONTH.nc' 
    fileobs  = argdict['dataobsdir'] + osp + 'dmondata_ice_NOAA.nc'
    return filename, fileobs

#=======================================================================

def _readnc(filenc=None,fileobs=None,argdict=myargs):
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['datemodel'] = rs.get_datetime(filenc)
    outdict['NVolume'   ] = rs.readfilenc(filenc, 'NVolume' )  / 1000
    outdict['NArea'     ] = rs.readfilenc(filenc, 'NArea' )    / 1000
    outdict['NExnsidc'  ] = rs.readfilenc(filenc, 'NExnsidc' ) / 1000
    outdict['SVolume'   ] = rs.readfilenc(filenc, 'SVolume' )  / 1000
    outdict['SArea'     ] = rs.readfilenc(filenc, 'SArea' )    / 1000
    outdict['SExnsidc'  ] = rs.readfilenc(filenc, 'SExnsidc' ) / 1000
    #
    outdict['dateobs' ] = rs.get_datetime(fileobs)
    outdict['NORTH_ICE_EXTENT'] = rs.readfilenc(fileobs, 'NORTH_ICE_EXTENT')
    outdict['NORTH_ICE_AREA']   = rs.readfilenc(fileobs, 'NORTH_ICE_AREA')
    outdict['SOUTH_ICE_EXTENT'] = rs.readfilenc(fileobs, 'SOUTH_ICE_EXTENT')
    outdict['SOUTH_ICE_AREA']   = rs.readfilenc(fileobs, 'SOUTH_ICE_AREA')
    # 
    return outdict # return the dictionnary of values 



#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):
    # load outdict
    for key in kwargs:
        exec(key+'=kwargs[key]')
    # get time limits
    tmin,tmax = ps.get_tminmax(datemodel)
    # 
    if figure is None: # by default create a new figure
        figure = plt.figure()
    #
    ax1 = figure.add_subplot(4,1,1)
    if not(compare) :
        plt.title(argdict['config'] + '-' + argdict['case']+'\n'+' Ice index for Northern and Southern hemisphere - Observations from NOAA/NSIDC (b) ' )
    else:
        plt.title(' Ice index for Northern and Southern hemisphere - Observations from NOAA/NSIDC (b) ' )
    ax1.plot(datemodel, NExnsidc, color, dateobs, NORTH_ICE_EXTENT, 'b--')
    vmin,vmax = ps.get_vminmax(NExnsidc,NORTH_ICE_EXTENT)
    ax1.axis([tmin,tmax,vmin,vmax])
    ax1.grid(True)
    plt.ylabel('North extent',fontsize='large')
    ps.set_dateticks(ax1)

    ax2 = figure.add_subplot(4,1,2)
    ax2.plot(datemodel, NArea, color, dateobs, NORTH_ICE_AREA, 'b--')
    vmin,vmax = ps.get_vminmax(NArea,NORTH_ICE_AREA)
    ax2.axis([tmin,tmax,vmin,vmax])
    ax2.grid(True)
    plt.ylabel('North area',fontsize='large')
    ps.set_dateticks(ax2)

    ax3 = figure.add_subplot(4,1,3)
    ax3.plot(datemodel, SExnsidc, color, dateobs, SOUTH_ICE_EXTENT, 'b--')
    vmin,vmax = ps.get_vminmax(SExnsidc,SOUTH_ICE_EXTENT)
    ax3.axis([tmin,tmax,vmin,vmax])
    ax3.grid(True)
    plt.ylabel('South extent',fontsize='large')
    ps.set_dateticks(ax3)

    ax4 = figure.add_subplot(4,1,4)
    ax4.plot(datemodel, SArea, color, dateobs, SOUTH_ICE_AREA, 'b--')
    vmin,vmax = ps.get_vminmax(SArea,SOUTH_ICE_AREA)
    ax4.axis([tmin,tmax,vmin,vmax])
    ax4.grid(True)
    plt.ylabel('South area',fontsize='large')
    ps.set_dateticks(ax4)
    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case'] 
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_icenoaa.png')

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
      fig.savefig('./icenoaa.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

