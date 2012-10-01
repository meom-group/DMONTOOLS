#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""tao_profiles : compare zonal velocity w/ TAO moorings for drakkar runs
"""
#-----------------------------------------------------------------------
#                       Additional Documentation
#
# Modification History:
#  - May 2010: Original module by R. Dussin and J. Le Sommer
#
# Copyright (c) 2010, R. Dussin, J. Le Sommer, D. Munier. For licensing, distribution
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

plot_name = 'tao_profiles'
fig_size =  [16.,8.] 

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
          dummy, file_obs, = _get_ncname(argdict=argdict)
          return _readnc(fromfile[0], file_obs, argdict=argdict)
       elif fromfile[0].endswith('.mtl'): # if mtlfile name is provided
             print 'mlt not supported'
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
             + argdict['case'] + '_1y_TAO.nc' 
    fileobs  = argdict['dataobsdir'] + osp + 'dmondata_tao-equator_clim_PMEL-NOAA.nc'
    return filename, fileobs

#=======================================================================

def _readnc(filenc=None,fileobs=None,argdict=myargs):

    import matplotlib.pylab as plt
    from numpy import nan
    outdict = {} # creates the dictionnary which will contain the arrays

    lon_obs = rs.readfilenc(fileobs,'nav_lon')
    lat_obs = rs.readfilenc(fileobs,'nav_lat')
    dep_obs = rs.readfilenc(fileobs,'depthu')
    uzo_obs = rs.readfilenc(fileobs,'vozocrtx')

    ### rebuild a dictionary

    ## 1. define moorings name
    ##    this is a bit heavy but it is necessary to split 
    ##    the data from 4d arrays of NC file

    lon_moor = lon_obs[0,:] ; moor_name = []
    ind_swap_EW = npy.where( lon_moor > 180. )
    lon_moor[ind_swap_EW] = 360. - lon_moor[ind_swap_EW]

    for km in range(len(lon_moor)):
        moor_name.append(str(int(lon_moor[km])) + 'e') # default lon is towards east

    for km in list(ind_swap_EW[0].data):
        moor_name[km] = moor_name[km].replace('e','w') # correct for western moorings 

    outdict['coord'] = moor_name

    ## 2. fill with observations data
    for km in range(len(moor_name)):
        outdict['u_'    + moor_name[km] + '_obs' ] = uzo_obs[:,km]
        outdict['dept_' + moor_name[km] + '_obs' ] = dep_obs[:,km]

    ## 3. model outputs
    outdict['dept_mod'] = rs.readfilenc(filenc,'depth')
    outdict['time_mod'] = rs.get_datetime(filenc)

    km = 0
    for moor in moor_name:
	try:
		temp = rs.readfilenc(filenc,'u_' + moor)
		if len(temp.shape) == 1:
			outdict['u_' + moor_name[km] + '_mod' ] = temp
		else:
			outdict['u_' + moor_name[km] + '_mod' ] = temp.mean(0)
	except:
		print 'fail to read mooring ', moor
		pass
	km = km + 1

    return outdict   #return the dictionnary of values


#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):
    #
    if figure is None: # by default create a new figure
        figure = plt.figure(figsize=fig_size)
    #
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #

    nbplots = len(coord)

    for kp in range(nbplots):
	plt.subplot(1,nbplots,kp+1)
	uzon_mod = 100. * vars()['u_'    + coord[kp] + '_mod'] # in cm/s
	uzon_obs = 100. * vars()['u_'    + coord[kp] + '_obs'] # in cm/s
	dept_obs =        vars()['dept_' + coord[kp] + '_obs']

	plt.plot(uzon_mod,-dept_mod,color,uzon_obs,-dept_obs,'bx')
        plt.axis([-40,120,-320,0])
        plt.grid(linestyle='-',linewidth=0.1)

	if not(compare) and kp==int(nbplots / 2.):
                plt.title('Mean profile of zonal currents ( model : ' 
                           + str( time_mod[0].year ) + '-' + str( time_mod[-1].year ) +  ' ) - ' 
                           + argdict['config'] + '-' + argdict['case']+'\n'+ coord[kp])
        else :
                plt.title(coord[kp])

	if kp==int(nbplots / 2.):
        	plt.xlabel('Velocity (cm/s)')
	if kp==0:
        	plt.ylabel('Depth (m)')

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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_1y_tao_profil.png')


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
      fig.savefig('./tao_profil.png')
if __name__ == '__main__':
    sys.exit(main() or 0)

