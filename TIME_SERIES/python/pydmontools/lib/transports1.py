#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""transports1 : monitoring of transports for drakkar runs
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
plot_name = 'transports1'
fig_size =  [17.,25.]
plt.rcParams.update({'figure.figsize': fig_size})


#=======================================================================
#--- Reading the data 
#=======================================================================

def read(argdict=myargs,fromfile=[]):
    if fromfile!=[]:                      # diagnostic mode
       if fromfile[0].endswith('.nc'):    # if ncfile name is provided
          if not(len(fromfile)==1):
             print 'please provide one nc filename'
             sys.exit() 
          return _readnc(fromfile[0],argdict=argdict) 
       elif fromfile[0].endswith('.mtl'): # if mtlfile name is provided
          print 'mtl files are no longer supported'
	  sys.exit()
       else:                               
          pass
    elif fromfile==[]:                    # production mode 
       filenc = _get_ncname(argdict=argdict)
       # try to open the netcdf file
       if os.path.isfile(filenc):
          return _readnc(filenc,argdict=argdict) 
          
def _get_ncname(argdict=myargs):
    fileroot = argdict['datadir'] + osp + argdict['config'] + '-' \
            + argdict['case'] 
    filename = fileroot + '_TRANSPORTS.nc'
    return filename

#=======================================================================

def _readnc(filenc=None,argdict=myargs):
    if argdict.has_key('compared_configs'):
       compare_different_configs = (len(argdict['compared_configs'])>1)
    else: 
       compare_different_configs = False # assigned to but never used, why ?  
    # get the section names corresponding to the config
    (truenames, shortnames, longnames, sens) = rs.define_sections(argdict)
    #
    nsection=len(truenames)
    outdict = {} # creates the dictionnary which will contain the arrays 
    date = rs.get_datetime(filenc)
    mass =[] ; heat=[] ; salt = []
    for kk in range(nsection):
        mass.append(rs.readfilenc(filenc, 'vtrp' + '_' + shortnames[kk]))
        heat.append(rs.readfilenc(filenc, 'htrp' + '_' + shortnames[kk]))
        salt.append(rs.readfilenc(filenc, 'strp' + '_' + shortnames[kk]))

    mass = npy.reshape(mass, [nsection, len(date)])
    heat = npy.reshape(heat, [nsection, len(date)])
    salt = npy.reshape(salt, [nsection, len(date)])

    bigsens = npy.transpose(sens * npy.ones((len(date),nsection)))

    outdict['date']    = date
    outdict['massplt'] = mass * bigsens
    outdict['heatplt'] = heat * bigsens
    outdict['saltplt'] = salt * bigsens

    return outdict # return the dictionnary of values 


#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None,color='r',massplt=None,heatplt=None,saltplt=None,date=None,compare=False):
    
    # adjust the figure size 
    (truenames, shortnames, longnames, sens) = rs.define_sections(argdict)
    nsection=len(truenames)
    fig_size = [18., float(nsection) * 5.]

    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)

    _date = ps.mdates.date2num(date) # now a numerical value
    for k in range(1,nsection+1) :
          ax1 = figure.add_subplot(nsection,3,3*(k-1)+1)
          ax1.plot(date, massplt[k-1,:], color + '.-')
          ax1.axis([min(_date),max(_date),min(massplt[k-1,:]),max(massplt[k-1,:])])
          ax1.grid(True)
          plt.ylabel(longnames[k-1].replace('_',' '),fontsize='small')
          ps.set_dateticks(ax1)
          if k==1 :
                plt.title('Mass Transport',fontsize='large')
          ax2 = figure.add_subplot(nsection,3,3*(k-1)+2)
          ax2.plot(date, heatplt[k-1,:], color + '.-')
          ax2.axis([min(date),max(date),min(heatplt[k-1,:]),max(heatplt[k-1,:])])
          ax2.grid(True)
          ps.set_dateticks(ax2)
          if not(compare) and k==1 :
                plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Heat Transport',fontsize='large')
          elif k==1 :
                plt.title('Heat Transport',fontsize='large')

          ax3 = figure.add_subplot(nsection,3,3*(k-1)+3)
          ax3.plot(date, saltplt[k-1,:], color + '.-')
          ax3.axis([min(date),max(date),min(saltplt[k-1,:]),max(saltplt[k-1,:])])
          plt.grid(True)
          ps.set_dateticks(ax3)
          if k==1 :
                plt.title('Salt Transport',fontsize='large')
          #figure.autofmt_xdate() # should be adapted a bit more...
  
    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_transports1.png')

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
      fig.savefig('./transports1.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

