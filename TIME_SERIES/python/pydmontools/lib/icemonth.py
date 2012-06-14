#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""icemonth : monitoring of ice volume, area & extent for drakkar runs
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

plot_name = 'icemonth'

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
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_1m_ICEMONTH.nc' 
    return filename

#=======================================================================

def _readnc(filenc=None, argdict=myargs):
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['datemodel'] = rs.get_datetime(filenc)
    outdict['NVolume'   ] = rs.readfilenc(filenc, 'NVolume' )
    outdict['NArea'     ] = rs.readfilenc(filenc, 'NArea' )
    outdict['NExnsidc'  ] = rs.readfilenc(filenc, 'NExnsidc' )
    outdict['SVolume'   ] = rs.readfilenc(filenc, 'SVolume' )
    outdict['SArea'     ] = rs.readfilenc(filenc, 'SArea' )
    outdict['SExnsidc'  ] = rs.readfilenc(filenc, 'SExnsidc' )
    #
    return outdict # return the dictionnary of values 


#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):
    #
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #
    if argdict['config'].find('ORCA') != -1 :
        north = 1 ; south = 1 
        print "icemonth.py : use default values (for global)"
    elif argdict['config'].find('PERIANT') != -1 :
        north = 0 ; south = 1
        print "icemonth.py : use custom values for PERIANT configs"
    elif argdict['config'].find('NATL') != -1 :
        north = 1 ; south = 0
        print "icemonth.py : use custom values for NATL configs"
    else :
        north = 1 ; south = 1 
        print "icemonth.py : Your config is not supported..."
        print "The monitoring will try to use default values from Global Configuration"
    #
    nbzone = 2
    nbplotline = 3
    fig_size = [float(nbplotline) * 6., float(nbzone) * 5.]
    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)

    #
    if north == 1 :
        ax=figure.add_subplot(nbzone,nbplotline,1)
        ax.plot(datemodel, NVolume, color)
        _datemodel = ps.mdates.date2num(datemodel) # now a numerical value
        ax.axis([min(_datemodel), max(_datemodel), 0, 60000])
        ax.grid(True)
        plt.ylabel('Northern',fontsize='large')
        plt.title('Ice Volume (10**9 m**3)',fontsize='large')
        ps.set_dateticks(ax)

        ax=figure.add_subplot(nbzone,nbplotline,2)
        ax.plot(datemodel, NArea, color)
        ax.grid(True)
        ax.axis([min(_datemodel), max(_datemodel), 0, 15000])
        ps.set_dateticks(ax)
        if not(compare) :
            plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Ice Area (10**9 m**2)',fontsize='large')
        else:
            plt.title('Ice Area (10**9 m**2)',fontsize='large')
        
        ax=figure.add_subplot(nbzone,nbplotline,3)
        ax.plot(datemodel, NExnsidc, color)
        ax.grid(True)
        ax.axis([min(_datemodel), max(_datemodel), 0, 20000])
        plt.title('Ice extent (10**9 m**2)',fontsize='large')
        ps.set_dateticks(ax)
        figure.autofmt_xdate()
        
    if south == 1 :  
        ax=figure.add_subplot(nbzone,nbplotline,4)
        ax.plot(datemodel, SVolume, color)
        ax.grid(True)
        _datemodel = ps.mdates.date2num(datemodel) # now a numerical value
        ax.axis([min(_datemodel), max(_datemodel), 0, 20000])
        plt.ylabel('Southern',fontsize='large')
        plt.title('Ice Volume (10**9 m**3)',fontsize='large')
        ps.set_dateticks(ax)

        ax=figure.add_subplot(nbzone,nbplotline,5)
        ax.plot(datemodel, SArea, color)
        ax.grid(True)
        ax.axis([min(_datemodel), max(_datemodel), 0, 20000])
        ps.set_dateticks(ax)
        if not(compare) :
            plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Ice Area (10**9 m**2)',fontsize='large')
        else:
            plt.title('Ice Area (10**9 m**2)',fontsize='large')

        ax=figure.add_subplot(nbzone,nbplotline,6)
        ax.plot(datemodel, SExnsidc, color)
        ax.grid(True)
        ax.axis([min(_datemodel), max(_datemodel), 0, 20000])
        plt.title('Ice extent (10**9 m**2)',fontsize='large')
        ps.set_dateticks(ax)
        figure.autofmt_xdate()
     
    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '1m_icemonth.png')

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
      fig.savefig('./icemonth.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

