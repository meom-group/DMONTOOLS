#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""mht1 : meridionnal heat transport for drakkar runs
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

plot_name = 'mht1'

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
          print '>>> mht1.py : No files found for frequency : ' + argdict['monitor_frequency'] ; exit()

          
def _get_ncname(argdict=myargs):
    #
    if rs.check_freq_arg(argdict['monitor_frequency']):

        filename = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_MHT.nc'

    return filename

#=======================================================================

def _readnc(filenc=None,argdict=myargs):
    #
    if argdict['config'].find('ORCA') == 0:
        list_field = ['zomht_glo', 'zomht_atl', 'zomht_inp','hflx_glo' , 'hflx_atl' , 'hflx_inp']
    else:
        list_field = ['zomht_glo', 'hflx_glo']
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['date']      = rs.get_datetime(filenc)
    outdict['lat']       = rs.readfilenc(filenc, 'nav_lat')
    for field in list_field:
        temp           = rs.readfilenc(filenc,field)
	## remove spurious areas
	if field.find('atl') != -1:
		ymax = npy.abs( outdict['lat'] - 70 ).argmin() # find index of 70N
		ymin = npy.abs( outdict['lat'] + 30 ).argmin() # find index of 30S
		temp[:,ymax:] = 0.
		temp[:,:ymin] = 0.

	if field.find('inp') != -1:
		ymin = npy.abs( outdict['lat'] + 30 ).argmin() # find index of 30S
		temp[:,:ymin] = 0.

        outdict[field] = rs.remove_spval(temp, 9999, 0) 
    return outdict # return the dictionnary of values 


#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None,color='r',compare=False, **kwargs):
    #
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #
    _date = ps.mdates.date2num(date) # now a numerical value
    #
    if argdict['config'].find('ORCA') == 0:
        list_field = ['zomht_glo', 'hflx_glo' , 'zomht_atl' , 'hflx_atl' , 'zomht_inp' , 'hflx_inp']
        zone       = ['Global ', 'Atlantic ', 'Indo-Pacific ']
        nbvar  = 2 ; nbzone = 3
    else:
        list_field = ['zomht_glo', 'hflx_glo']
        zone       = ['Global ']
        nbvar  = 2 ; nbzone = 1
    #
    fig_size=[(float(nbvar) + 1) * 6., float(nbzone) * 5. ]
    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)
    #
    if compare:
        return figure
    #
    variable      = ['Southward Accumulated Qnet', 'Advective MHT']
    #
    # setup the color range
    limits_1 = [ -1.0, 1.0 ] ; step_1 = 0.05
    limits_2 = [ -2.0, 2.0 ] ; step_2 = 0.1
    contours_1=npy.arange(limits_1[0],limits_1[1]+step_1,step_1)
    contours_2=npy.arange(limits_2[0],limits_2[1]+step_2,step_2)

    for k in range(1,nbzone+1) :
	# what variable to plot
	varplt = npy.transpose(vars()[list_field[2*k-2]])
	# mask spval
	varplt = rs.nc_add_spval(varplt, 0.)
	#
        ax1 = figure.add_subplot(nbzone,nbvar+1,3*(k-1)+1)
        plt.contourf(date, lat, varplt,contours_2)
        plt.colorbar(format='%.3f')
        #plt.contour(date, lat, varplt,[0.],linewith=2)
        ax1.grid(True)
        ax1.axis([min(_date), max(_date), min(lat), max(lat)])
        plt.ylabel(zone[k-1],fontsize='x-large')
        ps.set_dateticks(ax1)
        if k==1 :
            plt.title(variable[0],fontsize='large')

	# what variable to plot
	varplt = npy.transpose(vars()[list_field[2*k-1]])
	# mask spval
	varplt = rs.nc_add_spval(varplt, 0.)
	#
        ax2 = figure.add_subplot(nbzone,nbvar+1,3*(k-1)+2)
        plt.contourf(date, lat, varplt,contours_2)
        plt.colorbar(format='%.3f')
        ax2.grid(True)
        plt.axis([min(_date), max(_date), min(lat), max(lat)])
	ps.set_dateticks(ax2)
        if k==1 :
            if not(compare) :
                plt.title(argdict['config'] + '-' + argdict['case']+'\n'+variable[1],fontsize='large')
            else:
                plt.title(variable[1],fontsize='large')
	# what variable to plot
	varplt = npy.transpose(vars()[list_field[2*k-2]] - vars()[list_field[2*k-1]])
	# mask spval
	varplt = rs.nc_add_spval(varplt, 0.)
	#
        ax3 = figure.add_subplot(nbzone,nbvar+1,3*(k-1)+3)
        plt.contourf(date, lat, varplt,contours_1)
        plt.colorbar(format='%.3f')
        ax3.grid(True)
        ax3.axis([min(_date), max(_date), min(lat), max(lat)])
	ps.set_dateticks(ax3)
        if k==1 :
            plt.title('Second minus first',fontsize='large')
 
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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_' + monit_freq + '_mht1.png')

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
      fig.savefig('./mht1.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

