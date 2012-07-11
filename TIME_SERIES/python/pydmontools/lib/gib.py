#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""gib : temperature and salinity profile of med waters outflow compared with levitus for drakkar runs
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

plot_name = 'gib'
fig_size =  [15.,18.]
plt.rcParams.update({'figure.figsize': fig_size})
depthanom = 1200.
maxdepth  = 5000. # for plots
smin      = 34.5  # for plots
ncontour  = 75

#=======================================================================
#--- Reading the data 
#=======================================================================

def read(argdict=myargs,fromfiles=[]):
    if fromfiles!=[]:                      # diagnostic mode...
       if fromfiles[0].endswith('.nc'):    # if ncfile name is provided
          if not(len(fromfiles)==2):
             print 'please provide two nc filenames'
             sys.exit() 
          return _readnc(fromfiles[0], fromfiles[1]) 
       elif fromfiles[0].endswith('.mtl'): # if mtlfile name is provided
          print 'mtl files are no longer supported'
          sys.exit() 
       else:                               
          pass
    elif fromfiles==[]:                    #  production mode...
       filenc, levitus = _get_ncname(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(filenc) and os.path.isfile(levitus):
          return _readnc(filenc, levitus) 
       else:
          print '>>> gib.py : No files found for frequency : ' + argdict['monitor_frequency'] ; exit()

          
def _get_ncname(argdict=myargs):
    #
    if rs.check_freq_arg(argdict['monitor_frequency']):

        filename = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_TSGIB.nc' 

    filelevnc = argdict['datadir'] + osp + 'LEVITUS_y0000_1y_TSGIB.nc'
    return filename, filelevnc

#=======================================================================
 
def _readnc(filenc=None,levitus=None):
    # 
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['levels']   = rs.readfilenc(filenc,'gdept')           
    outdict['date']     = rs.get_datetime(filenc) 
    outdict['tmodel']   = rs.readfilenc(filenc,'mean_votemper')  #2D
    outdict['smodel']   = rs.readfilenc(filenc,'mean_vosaline')  
    outdict['tlev']     = rs.readfilenc(levitus,'mean_votemper') #1D
    outdict['slev']     = rs.readfilenc(levitus,'mean_vosaline')  
    return outdict # return the dictionnary of values 


#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs,figure=None,color='r',levels=None,date=None,tmodel=None,smodel=None,tlev=None,slev=None,compare=False):
    _date = ps.mdates.date2num(date) # now a numerical value
    tmodel = rs.remove_spval(tmodel, 999. , 0.)
    smodel = rs.remove_spval(smodel, 999. , 0.)
    tlev   = rs.remove_spval(tlev  , 999. , 0.)
    slev   = rs.remove_spval(slev  , 999. , 0.)
    tlev2D = npy.ones((len(date),1,)) * tlev
    slev2D = npy.ones((len(date),1,)) * slev
    
    if figure is None: # by default create a new figure
          figure = plt.figure()
    #
    
    plt.subplot(3,2,1)
    plt.plot(tlev, levels, 'b', tmodel[-1,:], levels, color)
    plt.grid(True)
    plt.axis([min(min(tmodel[-1,:]),min(tlev)), max(max(tmodel[-1,:]),max(tlev)),maxdepth,min(levels)])
    plt.ylabel('Depth')
    if not(compare) :
        plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Last Tgib profile 14W-10W, 36N-40N (blue is Levitus)')
    else:
        plt.title('Last Tgib profile 14W-10W, 36N-40N (blue is Levitus)')
    
    plt.subplot(3,2,3)
    plt.plot(slev, levels, 'b', smodel[-1,:], levels, color)
    plt.grid(True)
    plt.axis([smin, max(max(smodel[-1,:]),max(slev)),maxdepth,min(levels)])
    plt.ylabel('Depth')
    plt.title('Last Sgib profile 14W-10W, 36N-40N (blue is Levitus)')
    
    levelanom = rs.find_closest_level(levels, depthanom)
    lvlm1 = levelanom
    
    ax1 = figure.add_subplot(3,2,5)
    ax1.plot(date, (tmodel[:,lvlm1]-tlev[lvlm1]), color+'.-')
    ax1.grid(True)
    ax1.axis([min(_date), max(_date), min(tmodel[:,lvlm1]-tlev[lvlm1]), max(tmodel[:,lvlm1]-tlev[lvlm1])])
    plt.ylabel('T anom (1000m)')
    plt.title('Temperature Anomaly around '+str(depthanom)+' m')
    ps.set_dateticks(ax1)

    ax2 = figure.add_subplot(3,2,6)
    ax2.plot(date, (smodel[:,lvlm1]-slev[lvlm1]), color+'.-')
    ax2.grid(True)
    ax2.axis([min(_date), max(_date), min(smodel[:,lvlm1]-slev[lvlm1]), max(smodel[:,lvlm1]-slev[lvlm1])])
    plt.ylabel('S anom (1000m)')
    plt.title('Salinity Anomaly around ' +str(depthanom)+' m')
    ps.set_dateticks(ax2)

    if not(compare) :
        # 2D hovmuller
        ax3 = figure.add_subplot(3,2,2)
        plt.contourf(date, levels, npy.transpose(tmodel-tlev2D),ncontour)
        plt.colorbar()
        ax3.grid(True)
        ax3.axis([min(_date), max(_date),maxdepth,min(levels)])
        plt.title(argdict['config'] + '-' + argdict['case'])
        plt.ylabel('Temperature anomaly')
        ps.set_dateticks(ax3)

        # 2D hovmuller
        ax4 = figure.add_subplot(3,2,4)
        plt.contourf(date, levels, npy.transpose(smodel-slev2D),ncontour)
        plt.colorbar()
        ax4.grid(True)
        ax4.axis([min(_date), max(_date),maxdepth,min(levels)])
        plt.ylabel('Salinity anomaly')
        ps.set_dateticks(ax4)

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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_' + monit_freq + '_gib.png')

#=======================================================================
#--- main 
#=======================================================================

def main(): 
   # get arguments
   parser = pydmontools.standard_pydmt_script_parser()
   (options, args) = parser.parse_args()
   argdict = myargs
   argdict.update(vars(options)) # updated with the command line options
   infiles = args                # default value is an empty list
   # read, plot and save   
   values = read(argdict=argdict,fromfiles=infiles)
   fig = plot(**values)
   if len(args)==0:
      save(argdict=argdict,figure=fig)
   else:
      fig.savefig('./gib.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

