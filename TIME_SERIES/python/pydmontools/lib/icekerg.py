#!/usr/bin/python
#=======================================================================
#                        General Documentation

"""icekerg : time series of ice concentration and thickness in boxes near Kerguelen islands
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
plot_name = 'icekerg'
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
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_1m_KERG.nc'
    return filename

#=======================================================================

def _readnc(filenc=None,argdict=myargs):
    outdict = {} # creates the dictionnary which will contain the arrays 
    date = rs.get_datetime(filenc)
    outdict['date']    = date
    outdict['ice1kerg1plt'] = rs.readfilenc(filenc, 'mean_ileadfra_KERG4')
    outdict['ice1kerg2plt'] = rs.readfilenc(filenc, 'mean_iicethic_KERG4')

    return outdict # return the dictionnary of values 


#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):

    for key in kwargs:
        exec(key + '=kwargs[key]')
    
    # adjust the figure size 
    fig_size = [18., 10.]

    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)

    _date = ps.mdates.date2num(date) # now a numerical value
    
    ax1 = figure.add_subplot(2,1,1)
    ax1.plot(date, ice1kerg1plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(ice1kerg1plt[:]),max(ice1kerg1plt[:])])
    ax1.grid(True)
    plt.ylabel('Ice box (60-70S)',fontsize='small')
    ps.set_dateticks(ax1)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Ice thickness',fontsize='large')
    else :
          plt.title('Ice concentration',fontsize='large')

    ax2 = figure.add_subplot(2,1,2)
    ax2.plot(date, ice1kerg2plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(ice1kerg2plt[:]),max(ice1kerg2plt[:])])
    ax2.grid(True)
    plt.ylabel('Ice box (60-70S)',fontsize='small')
    ps.set_dateticks(ax2)
    plt.title('Ice thickness',fontsize='large')
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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_1m_icekerg.png')

#=======================================================================

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
      fig.savefig('./icekerg.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

