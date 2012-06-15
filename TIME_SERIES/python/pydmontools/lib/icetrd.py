#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""icetrd : monitoring of ice volume, area and extent compared with NOAA data for drakkar runs
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

plot_name = 'icetrd'

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
    #
    outdict['dateobs' ] = rs.get_datetime(fileobs)
    outdict['NORTH_ICE_EXTENT'] = rs.readfilenc(fileobs, 'NORTH_ICE_EXTENT')
    outdict['NORTH_ICE_AREA']   = rs.readfilenc(fileobs, 'NORTH_ICE_AREA')
    outdict['SOUTH_ICE_EXTENT'] = rs.readfilenc(fileobs, 'SOUTH_ICE_EXTENT')
    outdict['SOUTH_ICE_AREA']   = rs.readfilenc(fileobs, 'SOUTH_ICE_AREA')

    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================
# same than icemonth but for a particular month + data

#def plot(argdict=myargs, figure=None, color='r', compare=False, month=3,**kwargs):
def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):
    #
    month=3
    for key in kwargs:
        exec(key+'=kwargs[key]')
    # get time limits
    tmin,tmax = ps.get_tminmax(date_model)
    # configuration dependant settings
    if argdict['config'].find('ORCA') != -1 :
        north = 1 ; south = 1
        print "icetrd.py : use default values (for global)"
    elif argdict['config'].find('PERIANT') != -1 :
        north = 0 ; south = 1
        print "icetrd.py : use custom values for PERIANT configs"
    elif argdict['config'].find('NATL') != -1 :
        north = 1 ; south = 0
        print "icetrd.py : use custom values for NATL configs"
    else :
        print "icetrd.py : Your config is not supported..."
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

    zdate_model = npy.array( date_model )
    zdate_obs   = npy.array( dateobs )

    indM3 = rs.get_month_indexes(zdate_model,3) # march in model
    # time
    time_model_march = zdate_model[indM3]
    year_model_march = rs.year_from_date(time_model_march)
    # data
    list_model_march = ['NVolume', 'NArea','NExnsidc']
    for k in list_model_march:
        exec(k + '_march =' + k + '[indM3]')
    

    indO3 = rs.get_month_indexes(zdate_obs,3) # march in observations
    # time
    time_obs_march = zdate_obs[indO3]
    year_obs_march = rs.year_from_date(time_obs_march)
    # data
    list_obs_march = ['NORTH_ICE_EXTENT','NORTH_ICE_AREA']
    for k in list_obs_march:
        exec(k + '_march =' + k + '[indO3]')

    indM9 = rs.get_month_indexes(zdate_model,9) # september in model
    # time
    time_model_sept = zdate_model[indM9]
    year_model_sept = rs.year_from_date(time_model_sept)
    # data
    list_model_sept = ['SVolume', 'SArea','SExnsidc']
    for k in list_model_sept:
        exec(k + '_sept =' + k + '[indM9]')


    indO9 = rs.get_month_indexes(zdate_obs,9) # september in observations
    # time
    time_obs_sept = zdate_obs[indO9]
    year_obs_sept = rs.year_from_date(time_obs_sept)
    # data
    list_obs_sept = ['SOUTH_ICE_EXTENT','SOUTH_ICE_AREA']
    for k in list_obs_sept:
        exec(k + '_sept =' + k + '[indO9]')

    #
    if north == 1 :
        plt.subplot(nbzone,nbplotline,1)
        plt.plot(year_model_march, NVolume_march, color)
        plt.axis([min(year_model_march), max(year_model_march), 0, 60])
        plt.grid(True)
        plt.title('Volume Arctic ' + listmonth[(month-1)%12],fontsize='large')

        plt.subplot(nbzone,nbplotline,2)
        plt.plot(year_model_march, NArea_march, color, year_obs_march, NORTH_ICE_AREA_march, 'b.-')
        plt.grid(True)
        plt.axis([min(year_model_march), max(year_model_march),
                  min(min(NArea_march),min(NORTH_ICE_AREA_march)), max(max(NArea_march),max(NORTH_ICE_AREA_march))])
        if not(compare) : 
            plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Area Arctic ' + listmonth[(month-1)%12]+' - Obs. (b)',fontsize='large')
        else :
            plt.title('Area Arctic ' + listmonth[(month-1)%12]+' - Obs. (b)',fontsize='large')

        plt.subplot(nbzone,nbplotline,3)
        plt.plot(year_model_march, NExnsidc_march, color, year_obs_march, NORTH_ICE_EXTENT_march, 'b.-')
        plt.grid(True)
        plt.axis([min(year_model_march), max(year_model_march),
                  min(min(NExnsidc_march),min(NORTH_ICE_EXTENT_march)), max(max(NExnsidc_march),max(NORTH_ICE_EXTENT_march))])
        plt.title('Extent Arctic ' + listmonth[(month-1)%12]+' - Obs. (b)',fontsize='large')
    #
    if south == 1 :
        plt.subplot(nbzone,nbplotline,north*nbplotline + 1)
        plt.plot(year_model_sept, SVolume_sept, color)
        plt.grid(True)
        plt.axis([min(year_model_sept), max(year_model_sept), 0, 20])
        plt.title('Volume Antarctic ' + listmonth[(month+6-1)%12],fontsize='large')

        plt.subplot(nbzone,nbplotline,north*nbplotline + 2)
        plt.plot(year_model_sept, SArea_sept, color,year_obs_sept, SOUTH_ICE_AREA_sept, 'b.-')
        plt.grid(True)
        plt.axis([min(year_model_sept), max(year_model_sept),
                  min(min(SArea_sept),min(SOUTH_ICE_AREA_sept)), max(max(SArea_sept),max(SOUTH_ICE_AREA_sept))])
        if not(compare) : 
            plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Area Antarctic ' + listmonth[(month+6-1)%12]+' - Obs. (b)',fontsize='large')
        else :
            plt.title('Area Antarctic ' + listmonth[(month+6-1)%12]+' - Obs. (b)',fontsize='large')

        plt.subplot(nbzone,nbplotline,north*nbplotline + 3)
        plt.plot(year_model_sept, SExnsidc_sept, color, year_obs_sept, SOUTH_ICE_EXTENT_sept, 'b.-')
        plt.grid(True)
        plt.axis([min(year_model_sept), max(year_model_sept),
                  min(min(SExnsidc_sept),min(SOUTH_ICE_EXTENT_sept)), max(max(SExnsidc_sept),max(SOUTH_ICE_EXTENT_sept))])
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
   # march
#   fig = plot(argdict=argdict,month=3,**values)
   fig = plot(argdict=argdict,**values)
   if len(args)==0:
      save(argdict=argdict,figure=fig,suffix='1y_icetrd')
   else:
      fig.savefig('./icetrd.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

