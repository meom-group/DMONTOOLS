#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""icetrd_min : monitoring of ice volume, area and extent compared with NOAA data for drakkar runs
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

plot_name = 'icetrd_min'

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
       file_nc, file_obs = _get_ncname(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(file_nc) and os.path.isfile(file_obs):
          return _readnc(file_nc, file_obs, argdict=argdict) 
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_1m_ICEMONTH.nc' 
    fileobs  = argdict['dataobsdir'] + osp + 'dmondata_ice_NOAA.nc'
    return filename, fileobs

 
#=======================================================================

def _readnc(filenc=None,fileobs=None,argdict=myargs):
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['date_model'] = rs.get_datetime(filenc)
    outdict['NVolume'   ] = rs.readfilenc(filenc, 'NVolume' )  / 1000
    outdict['NArea'     ] = rs.readfilenc(filenc, 'NArea' )    / 1000
    outdict['NExnsidc'  ] = rs.readfilenc(filenc, 'NExnsidc' ) / 1000
    outdict['SVolume'   ] = rs.readfilenc(filenc, 'SVolume' )  / 1000
    outdict['SArea'     ] = rs.readfilenc(filenc, 'SArea' )    / 1000
    outdict['SExnsidc'  ] = rs.readfilenc(filenc, 'SExnsidc' ) / 1000
    # The following code block should also be adapted to the new date format :
    # waiting until RD modifies data_obs_DRAKKAR.nc file.
    #year_tmp  = rs.readfilenc(fileobs, 'YEAR_ICE_NORTH')
    #month_tmp = rs.readfilenc(fileobs, 'MONTH_ICE_NORTH')
    #year_obs_north = year_tmp + ( month_tmp - 0.5) / 12  # middle of the month
    #year_tmp  = rs.readfilenc(fileobs, 'YEAR_ICE_SOUTH')
    #month_tmp = rs.readfilenc(fileobs, 'MONTH_ICE_SOUTH')
    #year_obs_south = year_tmp + ( month_tmp - 0.5) / 12  # middle of the month

    dataobslist = ['NORTH_ICE_EXTENT', 'NORTH_ICE_AREA', 'SOUTH_ICE_EXTENT', 'SOUTH_ICE_AREA']
    outdict['NORTH_ICE_EXTENT'] = rs.readfilenc(fileobs, 'NORTH_ICE_EXTENT')
    outdict['NORTH_ICE_AREA']   = rs.readfilenc(fileobs, 'NORTH_ICE_AREA')
    outdict['SOUTH_ICE_EXTENT'] = rs.readfilenc(fileobs, 'SOUTH_ICE_EXTENT')
    outdict['SOUTH_ICE_AREA']   = rs.readfilenc(fileobs, 'SOUTH_ICE_AREA')
    
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

#=======================================================================
#--- Plotting the data 
#=======================================================================
# same than icemonth but for a particular month + data

#def plot(argdict=myargs, figure=None, color='r', compare=False, month=None,**kwargs):
def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):
    #
    month=9   # hard coded : easiest way for compare (jmm)
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #
    if argdict['config'].find('ORCA') != -1 :
        north = 1 ; south = 1
        print "icetrd_min.py : use default values (for global)"
    elif argdict['config'].find('PERIANT') != -1 :
        north = 0 ; south = 1
        print "icetrd_min.py : use custom values for PERIANT configs"
    elif argdict['config'].find('NATL') != -1 :
        north = 1 ; south = 0
        print "icetrd_min.py : use custom values for NATL configs"
    else :
        print "icetrd_min.py : Your config is not supported..."
        print "The monitoring will try to use default values from Global Configuration"
    #
    nbzone = north + south
    nbplotline = 3
    fig_size = [float(nbplotline) * 6., float(nbzone) * 5.]
    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)

    listmonth=['January','Febuary','March','April','May','June','July','August',
               'September','October','November','December']
    #
    # looking for month index :
    # this computation looks complicated but it allows to work with a time_counter
    # starting any month
    yeartmp_real = year_model[0:12]              # we look at the first year only
    yeartmp_int  = npy.array((yeartmp_real),'i') # define the corresponding integer array
    index_model = rs.get_index(yeartmp_real - yeartmp_int, (month - 0.5)/12 ) - 1
    #
    yeartmp_real = year_obs_north[0:12]          # we look at the first year only
    yeartmp_int  = npy.array((yeartmp_real),'i') # define the corresponding integer array
    index_obs_north = rs.get_index(yeartmp_real - yeartmp_int, (month - 0.5)/12 ) - 1
    #
    yeartmp_real = year_obs_south[0:12]          # we look at the first year only
    yeartmp_int  = npy.array((yeartmp_real),'i') # define the corresponding integer array
    month_2lookfor = (month + 6 - 0.5)/12 
    #
    if month_2lookfor >= 1:
        month_2lookfor = month_2lookfor -1. 
    index_obs_south = rs.get_index(yeartmp_real - yeartmp_int, month_2lookfor ) - 1
    #
    # this is a bit ugly : we take a slice of each tab
    year_model_6m = npy.array((year_model),'f')
    # Northern Hemisphere :
    list_toslice = ['year_model', 'NVolume', 'NArea','NExnsidc']
    for k in list_toslice:
        exec(k + '=' + k + '[index_model::12]')
    # Southern Hemisphere :
    list_toslice = ['year_model_6m', 'SVolume', 'SArea','SExnsidc']
    for k in list_toslice:
        exec(k + '=' + k + '[index_model+6::12]')
    # 
    list_toslice = ['year_obs_north','NORTH_ICE_EXTENT','NORTH_ICE_AREA']
    for k in list_toslice:
        exec(k + '=' + k + '[index_obs_north::12]')
    # 
    list_toslice = ['year_obs_south','SOUTH_ICE_EXTENT','SOUTH_ICE_AREA']
    for k in list_toslice:
        exec(k + '=' + k + '[index_obs_south::12]')

    #
    if north == 1 :
        plt.subplot(nbzone,nbplotline,1)
        plt.plot(year_model, NVolume, color)
        plt.axis([min(year_model), max(year_model), 0, 60])
        plt.grid(True)
        plt.title('Volume Arctic ' + listmonth[(month-1)%12],fontsize='large')

        plt.subplot(nbzone,nbplotline,2)
        plt.plot(year_model, NArea, color, year_obs_north, NORTH_ICE_AREA, 'b.-')
        plt.grid(True)
        plt.axis([min(year_model), max(year_model),
                  min(min(NArea),min(NORTH_ICE_AREA)), max(max(NArea),max(NORTH_ICE_AREA))])
        if not(compare) : 
            plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Area Arctic ' + listmonth[(month-1)%12]+' - Obs. (b)',fontsize='large')
        else :
            plt.title('Area Arctic ' + listmonth[(month-1)%12]+' - Obs. (b)',fontsize='large')

        plt.subplot(nbzone,nbplotline,3)
        plt.plot(year_model, NExnsidc, color, year_obs_north, NORTH_ICE_EXTENT, 'b.-')
        plt.grid(True)
        plt.axis([min(year_model), max(year_model),
                  min(min(NExnsidc),min(NORTH_ICE_EXTENT)), max(max(NExnsidc),max(NORTH_ICE_EXTENT))])
        plt.title('Extent Arctic ' + listmonth[(month-1)%12]+' - Obs. (b)',fontsize='large')
    #
    if south == 1 :
        plt.subplot(nbzone,nbplotline,north*nbplotline + 1)
        plt.plot(year_model_6m, SVolume, color)
        plt.grid(True)
        plt.axis([min(year_model_6m), max(year_model_6m), 0, 20])
        plt.title('Volume Antarctic ' + listmonth[(month+6-1)%12],fontsize='large')

        plt.subplot(nbzone,nbplotline,north*nbplotline + 2)
        plt.plot(year_model_6m, SArea, color,year_obs_south, SOUTH_ICE_AREA, 'b.-')
        plt.grid(True)
        plt.axis([min(year_model_6m), max(year_model_6m),
                  min(min(SArea),min(SOUTH_ICE_AREA)), max(max(SArea),max(SOUTH_ICE_AREA))])
        if not(compare) : 
            plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Area Antarctic ' + listmonth[(month+6-1)%12]+' - Obs. (b)',fontsize='large')
        else :
            plt.title('Area Antarctic ' + listmonth[(month+6-1)%12]+' - Obs. (b)',fontsize='large')

        plt.subplot(nbzone,nbplotline,north*nbplotline + 3)
        plt.plot(year_model_6m, SExnsidc, color, year_obs_south, SOUTH_ICE_EXTENT, 'b.-')
        plt.grid(True)
        plt.axis([min(year_model_6m), max(year_model_6m),
                  min(min(SExnsidc),min(SOUTH_ICE_EXTENT)), max(max(SExnsidc),max(SOUTH_ICE_EXTENT))])
        plt.title('Extent Antarctic ' + listmonth[(month+6-1)%12]+' - Obs. (b)',fontsize='large')


    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None,suffix=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_' + suffix + '.png')

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
   # September
#   fig = plot(argdict=argdict,month=9,**values)
   fig = plot(argdict=argdict,**values)
   if len(args)==0:
      save(argdict=argdict,figure=fig,suffix='1y_icetrd_min')
   else:
      fig.savefig('./icetrd_min.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

