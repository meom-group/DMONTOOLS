#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""nino : monitoring of el nino for drakkar runs
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

plot_name = 'nino'
fig_size =  [15.,18.]
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
          dummy, file_obs, file_obs2 = _get_ncname(argdict=argdict)
          return _readnc(fromfile[0], file_obs, file_obs2, argdict=argdict) 
       elif fromfile[0].endswith('.mtl'): # if mtlfile name is provided
          print 'mtl files are no longer supported'
	  sys.exit()
       else:                               
          pass
    elif fromfile==[]:                    # production mode 
       file_nc, file_obs, file_obs2 = _get_ncname(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(file_nc) and os.path.isfile(file_obs) and os.path.isfile(file_obs2):
          return _readnc(file_nc, file_obs, file_obs2, argdict=argdict) 
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_1m_NINO.nc' 
    fileobs  = argdict['dataobsdir'] + osp + 'dmondata_ninoboxes_CPC-NOAA.nc'
    fileobs2 = argdict['dataobsdir'] + osp + 'data_obs_DRAKKAR.nc'
    return filename, fileobs, fileobs2

 
#=======================================================================

def _readnc(filenc=None,fileobs=None,fileobs2=None,argdict=myargs):
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['datemodel'  ]  = rs.get_datetime(filenc)
    outdict['NINO12_model']  = rs.readfilenc(filenc, 'mean_votemper_NINO12' )
    outdict['NINO34_model']  = rs.readfilenc(filenc, 'mean_votemper_NINO34' )
    outdict['NINO3_model' ]  = rs.readfilenc(filenc, 'mean_votemper_NINO3'  )
    outdict['NINO4_model' ]  = rs.readfilenc(filenc, 'mean_votemper_NINO4'  )
    #
    outdict['dateobs']       = rs.get_datetime(fileobs)
    outdict['NINO12_obs'  ]  = rs.readfilenc(fileobs, 'NINO1+2' )
    outdict['NINO34_obs'  ]  = rs.readfilenc(fileobs, 'NINO3.4' )
    outdict['NINO3_obs'   ]  = rs.readfilenc(fileobs, 'NINO3'   )
    outdict['NINO4_obs'   ]  = rs.readfilenc(fileobs, 'NINO4'   )

    #outdict['SOI'         ]  = rs.readfilenc(fileobs2, 'SOI')
    
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
    # get time limits
    tmin,tmax = ps.get_tminmax(dateobs)
    #
    ax1 = figure.add_subplot(5,1,1)
    if not(compare) :
           plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'SST in the tropical Pacific - Observation (b)') 
    else:
           plt.title('SST in the tropical Pacific - Observation (b)') 
    ax1.plot(datemodel, NINO12_model, color, dateobs, NINO12_obs, 'b')
    vmin,vmax = ps.get_vminmax(NINO12_model,NINO12_obs)
    ax1.axis([tmin,tmax,vmin,vmax])
    ax1.grid(True)
    plt.ylabel('Nino1+2', fontsize='small')
    ps.set_dateticks(ax1)

    ax2 = figure.add_subplot(5,1,2)
    ax2.plot(datemodel, NINO3_model, color, dateobs, NINO3_obs, 'b')
    vmin,vmax = ps.get_vminmax(NINO3_model,NINO3_obs)
    ax2.axis([tmin,tmax,vmin,vmax])
    ax2.grid(True)
    plt.ylabel('Nino3', fontsize='small')
    ps.set_dateticks(ax2)

    ax3 = figure.add_subplot(5,1,3)
    ax3.plot(datemodel, NINO4_model, color, dateobs, NINO4_obs, 'b')
    vmin,vmax = ps.get_vminmax(NINO4_model,NINO4_obs)
    ax3.axis([tmin,tmax,vmin,vmax])
    ax3.grid(True)
    plt.ylabel('Nino4', fontsize='small')
    ps.set_dateticks(ax3)

    ax4 = figure.add_subplot(5,1,4)
    ax4.plot(datemodel, NINO34_model, color, dateobs, NINO34_obs, 'b')
    vmin,vmax = ps.get_vminmax(NINO34_model,NINO34_obs)
    ax4.axis([tmin,tmax,vmin,vmax])
    ax4.grid(True)
    plt.ylabel('Nino3.4', fontsize='small')
    ps.set_dateticks(ax4)

    #ax5 = figure.add_subplot(5,1,5)
    #ax5.plot(dateobs, SOI, 'b')
    #vmin,vmax = ps.get_vminmax(SOI,SOI) # a bit weird...
    #ax5.axis([tmin,tmax,vmin,vmax])
    #ax5.grid(True)
    #plt.ylabel('SO Index', fontsize='small')
    #ps.set_dateticks(ax5)
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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_1m_nino.png')

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
      fig.savefig('./nino.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

