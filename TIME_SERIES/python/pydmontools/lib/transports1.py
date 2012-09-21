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

# reference salinity for freshwater transport
Sref = 35.0

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
       else:
          print '>>> transports1.py : No files found for frequency : ' + argdict['monitor_frequency'] ; exit()

          
def _get_ncname(argdict=myargs):
    #
    if rs.check_freq_arg(argdict['monitor_frequency']):

        filename = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_TRANSPORTS.nc'

    return filename

#=======================================================================

def _readnc(filenc=None,argdict=myargs):
    if argdict.has_key('compared_configs'):
       compare = True
    else: 
       argdict['compared_configs'] = [ argdict['config'] ]
       compare = False
    # get the section names corresponding to the config
    section_dict = rs.define_all_sections(argdict,compare)
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    date = rs.get_datetime(filenc)
    outdict['date']    = date

    list_truenames = section_dict.keys()
    list_truenames.sort()

    for section in list_truenames:

       shortname = section_dict[section]['shortname']  # temporary
       sens      = section_dict[section]['sens']       # variables

       try:
           outdict['mass_' + shortname] = npy.array((sens),'f') * rs.readfilenc(filenc, 'vtrp' + '_' + shortname)
           outdict['heat_' + shortname] = npy.array((sens),'f') * rs.readfilenc(filenc, 'htrp' + '_' + shortname)
           outdict['salt_' + shortname] = npy.array((sens),'f') * rs.readfilenc(filenc, 'strp' + '_' + shortname)
       except:
           outdict['mass_' + shortname] = npy.zeros(( len(outdict['date']) ))
           outdict['heat_' + shortname] = npy.zeros(( len(outdict['date']) ))
           outdict['salt_' + shortname] = npy.zeros(( len(outdict['date']) ))

    return outdict # return the dictionnary of values 





#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):

    for key in kwargs:
        exec(key + '=kwargs[key]')

    # adjust the figure size 
    section_dict = rs.define_sections(argdict,compare)
    nsection=len(section_dict.keys())
    fig_size = [18., float(nsection) * 5.]

    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)

    _date = ps.mdates.date2num(date) # now a numerical value
    list_truenames = section_dict.keys()
    list_truenames.sort()

    pltline = 0 # counter for plot line

    nonsense = 9e15 # the greatest trick : for compare mode set min and max
                    # default values to non-sense so that they will be replaced by following plot

    for section in list_truenames:

          shortname = section_dict[section]['shortname']
          longname  = section_dict[section]['longname']

          exec('massplt =' + 'mass_' + shortname )
          exec('heatplt =' + 'heat_' + shortname )
          exec('saltplt =' + 'salt_' + shortname )

          ### mass transport
          ax1 = figure.add_subplot(nsection, 3, 3*pltline + 1)
          ax1.axis([min(_date),max(_date),nonsense,-nonsense])
          if massplt.mean() != 0.:
              ax1.plot(date, massplt, color + '.-')
              ax1.axis([min(_date),max(_date),min(massplt),max(massplt)])
          ax1.grid(True)
          plt.ylabel(longname.replace('_',' '),fontsize='small')
          ps.set_dateticks(ax1)
          if pltline==0 :
                plt.title('Mass Transport',fontsize='large')

          ### heat transport
          ax2 = figure.add_subplot(nsection, 3, 3*pltline + 2)
          ax2.axis([min(_date),max(_date),nonsense,-nonsense])
          if heatplt.mean() != 0.:
              ax2.plot(date, heatplt, color + '.-')
              ax2.axis([min(_date),max(_date),min(heatplt),max(heatplt)])
          ax2.grid(True)
          ps.set_dateticks(ax2)

          if not(compare) and pltline==0:
                plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Heat Transport',fontsize='large')
          elif pltline==0 :
                plt.title('Heat Transport',fontsize='large')

          ### freshwater transport
          ax3 = figure.add_subplot(nsection, 3, 3*pltline + 3)
          ax3.axis([min(_date),max(_date),nonsense,-nonsense])
          if saltplt.mean() != 0.:
              freshwaterplt = (saltplt - (Sref * massplt)) / Sref
              ax3.plot(date, freshwaterplt, color + '.-')
              ax3.axis([min(_date),max(_date),min(freshwaterplt),max(freshwaterplt)])
          ax3.grid(True)
          ps.set_dateticks(ax3)
          if pltline==0 :
                plt.title('Freshwater Transport - ref salinity = ' + str(Sref) + ' PSU',fontsize='large')
          #figure.autofmt_xdate() # should be adapted a bit more...
          pltline = pltline + 1

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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_' + monit_freq + '_transports1.png')

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

