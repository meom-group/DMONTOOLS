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
          if not(len(fromfile)==1):
             print 'please provide one mlt filename'
             sys.exit() 
          dummy, file_obs = _get_mtlnames(argdict=argdict)
          return _readmtl(fromfile[0], file_obs, argdict=argdict)
       else:                               
          pass
    elif fromfile==[]:                    # production mode 
       file_nc, file_obs  = _get_ncname(argdict=argdict)
       file_mtl,file_obs  = _get_mtlnames(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(file_nc) and os.path.isfile(file_obs):
          return _readnc(file_nc, file_obs, argdict=argdict) 
       # or try the mlt version   
       elif os.path.isfile(file_mtl) and os.path.isfile(file_obs):
          return _readmtl(file_mtl, file_obs, argdict=argdict)
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_ICEMONTH.nc' 
    fileobs  = argdict['dataobsdir'] + osp + 'data_obs_DRAKKAR.nc'
    return filename, fileobs

def _get_mtlnames(argdict=myargs):
    filemtl  = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_icemonth.mtl' 
    fileobs  = argdict['dataobsdir'] + osp + 'data_obs_DRAKKAR.nc'
    return filemtl , fileobs
 
#=======================================================================

def _readnc(filenc=None,fileobs=None,argdict=myargs):
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['year_model'] = rs.get_years(filenc)
    outdict['NVolume'   ] = rs.readfilenc(filenc, 'NVolume' )  / 1000
    outdict['NArea'     ] = rs.readfilenc(filenc, 'NArea' )    / 1000
    outdict['NExnsidc'  ] = rs.readfilenc(filenc, 'NExnsidc' ) / 1000
    outdict['SVolume'   ] = rs.readfilenc(filenc, 'SVolume' )  / 1000
    outdict['SArea'     ] = rs.readfilenc(filenc, 'SArea' )    / 1000
    outdict['SExnsidc'  ] = rs.readfilenc(filenc, 'SExnsidc' ) / 1000
    #
    year_tmp  = rs.readfilenc(fileobs, 'YEAR_ICE_NORTH')
    month_tmp = rs.readfilenc(fileobs, 'MONTH_ICE_NORTH')
    year_obs_north = year_tmp + ( month_tmp - 0.5) / 12  # middle of the month
    year_tmp  = rs.readfilenc(fileobs, 'YEAR_ICE_SOUTH')
    month_tmp = rs.readfilenc(fileobs, 'MONTH_ICE_SOUTH')
    year_obs_south = year_tmp + ( month_tmp - 0.5) / 12  # middle of the month

    dataobslist = ['NORTH_ICE_EXTENT', 'NORTH_ICE_AREA', 'SOUTH_ICE_EXTENT', 'SOUTH_ICE_AREA']
    NORTH_ICE_EXTENT = rs.readfilenc(fileobs, 'NORTH_ICE_EXTENT')
    NORTH_ICE_AREA   = rs.readfilenc(fileobs, 'NORTH_ICE_AREA')
    SOUTH_ICE_EXTENT = rs.readfilenc(fileobs, 'SOUTH_ICE_EXTENT')
    SOUTH_ICE_AREA   = rs.readfilenc(fileobs, 'SOUTH_ICE_AREA')
    
    ### This correction has already been done when we have created the data_obs_DRAKKAR.nc file
    # Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after ( Change of satellite SSM/R SSM/I )
    #indN = rs.get_index(year_obs_north, 1987.45)
    #for k in range(indN+1) :
    #    NORTH_ICE_AREA[k] = NORTH_ICE_AREA[k] + 1.19
    #for k in range(indN+1,len(NORTH_ICE_AREA)) :
    #    NORTH_ICE_AREA[k] = NORTH_ICE_AREA[k] + 0.31

    outdict['year_obs_north' ] = year_obs_north
    outdict['year_obs_south' ] = year_obs_south
    for k in dataobslist:
       exec('outdict[k] = ' + k )
    # 
    return outdict # return the dictionnary of values 


def _readmtl(filemtl=None, fileobs=None, argdict=myargs):
    #
    lignes  = rs.mtl_flush(filemtl)
    #
    datalist  = ['year_model','NVolume','SVolume','NArea','SArea','NExnsidc','SExnsidc']
    nmonth=12
    #
    for k in datalist:
        exec(k+'=[]') # init to empty array
    #
    for chaine in lignes[3:] :
        element=chaine.split()
        for k in range(1,1+nmonth) :
            year_model.append(float(element[0]) + ((float(k)-0.5)/nmonth) )
            NVolume.append(float(element[k])/1000) 
        for k in range( 1+nmonth,1+(2*nmonth) ) :
            SVolume.append(float(element[k])/1000) 
        for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
            NArea.append(float(element[k])/1000) 
        for k in range( 1+(3*nmonth),1+(4*nmonth) ) :
            SArea.append(float(element[k])/1000) 
        for k in range( 1+(4*nmonth),1+(5*nmonth) ) :
            NExnsidc.append(float(element[k])/1000) 
        for k in range( 1+(5*nmonth),1+(6*nmonth) ) :
            SExnsidc.append(float(element[k])/1000) 
    #
    outdict = {}
    for k in datalist:
       exec('outdict[k] = ' + k )
    #
    year_tmp  = rs.readfilenc(fileobs, 'YEAR_ICE_NORTH')
    month_tmp = rs.readfilenc(fileobs, 'MONTH_ICE_NORTH')
    year_obs_north = year_tmp + ( month_tmp - 0.5) / 12  # middle of the month
    year_tmp  = rs.readfilenc(fileobs, 'YEAR_ICE_SOUTH')
    month_tmp = rs.readfilenc(fileobs, 'MONTH_ICE_SOUTH')
    year_obs_south = year_tmp + ( month_tmp - 0.5) / 12  # middle of the month

    dataobslist = ['NORTH_ICE_EXTENT', 'NORTH_ICE_AREA', 'SOUTH_ICE_EXTENT', 'SOUTH_ICE_AREA']
    NORTH_ICE_EXTENT = rs.readfilenc(fileobs, 'NORTH_ICE_EXTENT')
    NORTH_ICE_AREA   = rs.readfilenc(fileobs, 'NORTH_ICE_AREA')
    SOUTH_ICE_EXTENT = rs.readfilenc(fileobs, 'SOUTH_ICE_EXTENT')
    SOUTH_ICE_AREA   = rs.readfilenc(fileobs, 'SOUTH_ICE_AREA')

    ### This correction has already been done when we have created the data_obs_DRAKKAR.nc file
    # Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after ( Change of satellite SSM/R SSM/I )
    #indN = rs.get_index(year_obs_north, 1987.45)
    #for k in range(indN+1) :
    #    NORTH_ICE_AREA[k] = NORTH_ICE_AREA[k] + 1.19
    #for k in range(indN+1,len(NORTH_ICE_AREA)) :
    #    NORTH_ICE_AREA[k] = NORTH_ICE_AREA[k] + 0.31

    outdict['year_obs_north' ] = year_obs_north
    outdict['year_obs_south' ] = year_obs_south
    for k in dataobslist:
       exec('outdict[k] = ' + k )

    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):
    #
    if figure is None: # by default create a new figure
        figure = plt.figure()
    #
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #
    datalist_model  = ['NExnsidc','NArea','SExnsidc','SArea']
    datalist_obs    = ['NORTH_ICE_EXTENT','NORTH_ICE_AREA', 'SOUTH_ICE_EXTENT','SOUTH_ICE_AREA']
    #
    plt.subplot(4,1,1)
    plt.title(' Ice index for Northern and Southern hemisphere - Observations from NOAA/NSIDC (b) ' )
    plt.plot(year_model, NExnsidc, color, year_obs_north, NORTH_ICE_EXTENT, 'b--')
    plt.axis([min(year_model), max(year_model), 
             min(min(NExnsidc),min(NORTH_ICE_EXTENT)), max(max(NExnsidc),max(NORTH_ICE_EXTENT))])
    plt.grid(True)
    plt.ylabel('North extent',fontsize='large')

    plt.subplot(4,1,2)
    plt.plot(year_model, NArea, color, year_obs_north, NORTH_ICE_AREA, 'b--')
    plt.axis([min(year_model), max(year_model), 
             min(min(NArea),min(NORTH_ICE_AREA)), max(max(NArea),max(NORTH_ICE_AREA))])
    plt.grid(True)
    plt.ylabel('North area',fontsize='large')

    plt.subplot(4,1,3)
    plt.plot(year_model, SExnsidc, color, year_obs_south, SOUTH_ICE_EXTENT, 'b--')
    plt.axis([min(year_model), max(year_model), 
             min(min(SExnsidc),min(SOUTH_ICE_EXTENT)), max(max(SExnsidc),max(SOUTH_ICE_EXTENT))])
    plt.grid(True)
    plt.ylabel('South extent',fontsize='large')

    plt.subplot(4,1,4)
    plt.plot(year_model, SArea, color, year_obs_south, SOUTH_ICE_AREA, 'b--')
    plt.axis([min(year_model), max(year_model), 
             min(min(SArea),min(SOUTH_ICE_AREA)), max(max(SArea),max(SOUTH_ICE_AREA))])
    plt.grid(True)
    plt.ylabel('South area',fontsize='large')

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

