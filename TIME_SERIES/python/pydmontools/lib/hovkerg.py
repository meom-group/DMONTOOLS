#!/usr/bin/python
#=======================================================================
#                        General Documentation

"""hovkerg : temporal evolution of temperature, salinity and density profiles in Kerguelen Stations
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

plot_name = 'hovkerg'
fig_size =  [15.,18.]
plt.rcParams.update({'figure.figsize': fig_size})

#=======================================================================
#--- Reading the data 
#=======================================================================

def read(argdict=myargs,fromfiles=[]):
    if fromfiles!=[]:                      # diagnostic mode... 
       if fromfiles[0].endswith('.nc'):    # if ncfile name is provided
          if not(len(fromfiles)==1):
             print 'please provide one netcdf filename'
             sys.exit() 
          return _readnc(fromfiles[0])
       elif fromfiles[0].endswith('.mtl'): # if mtlfile name is provided
          print 'mtl files are no longer supported'
	  sys.exit()
       else:                               
          pass
    elif fromfiles==[]:                    # production mode... 
       filenc   = _get_ncname(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(filenc):
          return _readnc(filenc) 
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_1m_KERG.nc' 
    return filename


#======================================================================= 

def _readnc(filenc=None):
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['tprof3']   = rs.readfilenc(filenc,'mean_votemper_KERG3') 
    outdict['sprof3']   = rs.readfilenc(filenc,'mean_vosaline_KERG3') 
    outdict['dprof3']   = rs.readfilenc(filenc,'mean_vosigma0_KERG3') 
    outdict['mld3']     = rs.readfilenc(filenc,'mean_somxl010_KERG3') 
    outdict['tprof1']   = rs.readfilenc(filenc,'mean_votemper_KERG1') 
    outdict['sprof1']   = rs.readfilenc(filenc,'mean_vosaline_KERG1') 
    outdict['dprof1']   = rs.readfilenc(filenc,'mean_vosigma0_KERG1') 
    outdict['mld1']     = rs.readfilenc(filenc,'mean_somxl010_KERG1') 
    outdict['prof']     = rs.readfilenc(filenc,'gdept') 
    outdict['date']     = rs.get_datetime(filenc) 
    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):

    for key in kwargs:
        exec(key + '=kwargs[key]')
   
    _date = ps.mdates.date2num(date) # now a numerical value
    #75=[0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5]
    #75=[33.7,33.8,33.9,34,34.1,34.2,34.3,34.4,34.5,34.6,34.7,34.8]
    #75=[26.8,26.9,27,27.1,27.2,27.3,27.4,27.5,27.6,27.7,27.8]
    #75=[0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7]
    #75=[33.7,33.8,33.9,34,34.1,34.2,34.3,34.4,34.5,34.6,34.7,34.8]
    #75=[26.8,26.9,27,27.1,27.2,27.3,27.4,27.5,27.6,27.7,27.8]

    if figure is None: # by default create a new figure
          figure = plt.figure()
    
    ax1=figure.add_subplot(6,1,1)
    plt.contourf(date,prof,npy.transpose(npy.squeeze(tprof1)),75)
    plt.plot(date,npy.squeeze(mld1),'k')
    plt.colorbar(format='%.3f')
    plt.grid(True)
    plt.axis([min(_date), max(_date), 500, 0])
    plt.ylabel('Kerfix Station (68-69E 50-51S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Temperature profile',fontsize='large')

    ax2=figure.add_subplot(6,1,2)
    plt.contourf(date,prof,npy.transpose(npy.squeeze(sprof1)),75)
    plt.plot(date,npy.squeeze(mld1),'k')
    plt.colorbar(format='%.3f')
    plt.grid(True)
    plt.axis([min(_date), max(_date), 500, 0])
    plt.ylabel('Kerfix Station (68-69E 50-51S)',fontsize='small')
    ps.set_dateticks(ax2)
    plt.title('Salinity profile',fontsize='large')
    
    ax3=figure.add_subplot(6,1,3)
    plt.contourf(date,prof,npy.transpose(npy.squeeze(dprof1)),75)
    plt.plot(date,npy.squeeze(mld1),'k')
    plt.colorbar(format='%.3f')
    plt.grid(True)
    plt.axis([min(_date), max(_date), 500, 0])
    plt.ylabel('Kerfix Station (68-69E 50-51S)',fontsize='small')
    ps.set_dateticks(ax3)
    plt.title('Density profile',fontsize='large')

    ax4=figure.add_subplot(6,1,4)
    plt.contourf(date,prof,npy.transpose(npy.squeeze(tprof3)),75)
    plt.plot(date,npy.squeeze(mld3),'k')
    plt.colorbar(format='%.3f')
    plt.grid(True)
    plt.axis([min(_date), max(_date), 500, 0])
    plt.ylabel('Plume Station (80-82E 50-52S)',fontsize='small')
    ps.set_dateticks(ax4)
    plt.title('Temperature profile',fontsize='large')

    ax5=figure.add_subplot(6,1,5)
    plt.contourf(date,prof,npy.transpose(npy.squeeze(sprof3)),75)
    plt.plot(date,npy.squeeze(mld3),'k')
    plt.colorbar(format='%.3f')
    plt.grid(True)
    plt.axis([min(_date), max(_date), 500, 0])
    plt.ylabel('Plume Station (80-82E 50-52S)',fontsize='small')
    ps.set_dateticks(ax5)
    plt.title('Salinity profile',fontsize='large')
    
    ax6=figure.add_subplot(6,1,6)
    plt.contourf(date,prof,npy.transpose(npy.squeeze(dprof3)),75)
    plt.plot(date,npy.squeeze(mld3),'k')
    plt.colorbar(format='%.3f')
    plt.grid(True)
    plt.axis([min(_date), max(_date), 500, 0])
    plt.ylabel('Plume Station (80-82E 50-52S)',fontsize='small')
    ps.set_dateticks(ax6)
    plt.title('Density profile',fontsize='large')
 
    return figure

#=======================================================================
#--- Save the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_1m_hovkerg.png')

#=======================================================================
#--- main 
#=======================================================================

def main(): 
   # get arguments
   parser = pydmontools.standard_pydmt_script_parser()
   (options, args) = parser.parse_args()
   # default argument dictionnary
   argdict = myargs 
   # updated with the command line options
   argdict.update(vars(options))
   #
   infiles = args     # default value is an empty list
   # read, plot and save   
   values = read(argdict=argdict,fromfiles=infiles)
   fig = plot(**values)
   if len(args)==0:
      save(argdict=argdict,figure=fig)
   else:
      fig.savefig('./hovkerg.png')

if __name__ == '__main__':
    sys.exit(main() or 0)


