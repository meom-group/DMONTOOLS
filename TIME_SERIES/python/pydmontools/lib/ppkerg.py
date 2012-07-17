#!/usr/bin/python
#=======================================================================
#                        General Documentation

"""ppkerg : time series of mixed-layer depth in boxes near Kerguelen islands
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
plot_name = 'ppkerg'
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
             + argdict['case'] + '_1m_KERGb.nc'
    return filename

#=======================================================================

def _readnc(filenc=None,argdict=myargs):
    outdict = {} # creates the dictionnary which will contain the arrays 
    date = rs.get_datetime(filenc)
    outdict['date']    = date
    outdict['dppkerg1plt'] = rs.readfilenc(filenc, 'mean_3DPPPHY2_KERG1')
    outdict['dppkerg2plt'] = rs.readfilenc(filenc, 'mean_3DPPPHY2_KERG3')
    outdict['dppkerg3plt'] = rs.readfilenc(filenc, 'mean_3DPPPHY2_KERG5')
    outdict['dppkerg4plt'] = rs.readfilenc(filenc, 'mean_3DPPPHY2_KERG6')
    outdict['nppkerg1plt'] = rs.readfilenc(filenc, 'mean_3DPPPHY_KERG1')
    outdict['nppkerg2plt'] = rs.readfilenc(filenc, 'mean_3DPPPHY_KERG3')
    outdict['nppkerg3plt'] = rs.readfilenc(filenc, 'mean_3DPPPHY_KERG5')
    outdict['nppkerg4plt'] = rs.readfilenc(filenc, 'mean_3DPPPHY_KERG6')

    return outdict # return the dictionnary of values 


#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):

    for key in kwargs:
        exec(key + '=kwargs[key]')
    
    # adjust the figure size 
    fig_size = [18., 18.]

    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)

    _date = ps.mdates.date2num(date) # now a numerical value
    
    ax1 = figure.add_subplot(4,3,1)
    ax1.plot(date, dppkerg1plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(dppkerg1plt[:]),max(dppkerg1plt[:])])
    ax1.grid(True)
    plt.ylabel('Kerfix box (68-69E 50-51S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('DProd',fontsize='large')
    ax2 = figure.add_subplot(4,3,2)
    ax2.plot(date, nppkerg1plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(nppkerg1plt[:]),max(nppkerg1plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'NProd',fontsize='large')
    else :
          plt.title('NProd',fontsize='large')
    ax3 = figure.add_subplot(4,3,3)
    ax3.plot(date, dppkerg1plt[:]+nppkerg1plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(dppkerg1plt[:]+nppkerg1plt[:]),max(dppkerg1plt[:]+nppkerg1plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('Prod',fontsize='large')
    figure.autofmt_xdate() # should be adapted a bit more...
    
    ax1 = figure.add_subplot(4,3,4)
    ax1.plot(date, dppkerg2plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(dppkerg2plt[:]),max(dppkerg2plt[:])])
    ax1.grid(True)
    plt.ylabel('Plume3 box (80-82E 50-52S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('DProd',fontsize='large')
    ax2 = figure.add_subplot(4,3,5)
    ax2.plot(date, nppkerg2plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(nppkerg2plt[:]),max(nppkerg2plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'NProd',fontsize='large')
    else :
          plt.title('NProd',fontsize='large')
    ax3 = figure.add_subplot(4,3,6)
    ax3.plot(date, dppkerg2plt[:]+nppkerg2plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(dppkerg2plt[:]+nppkerg2plt[:]),max(dppkerg2plt[:]+nppkerg2plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('Prod',fontsize='large')
    figure.autofmt_xdate() # should be adapted a bit more...
  
    ax1 = figure.add_subplot(4,3,7)
    ax1.plot(date, dppkerg3plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(dppkerg3plt[:]),max(dppkerg3plt[:])])
    ax1.grid(True)
    plt.ylabel('Summer max MLD Box (20-120E 52-60S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('DProd',fontsize='large')
    ax2 = figure.add_subplot(4,3,8)
    ax2.plot(date, nppkerg3plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(nppkerg3plt[:]),max(nppkerg3plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'NProd',fontsize='large')
    else :
          plt.title('NProd',fontsize='large')
    ax3 = figure.add_subplot(4,3,9)
    ax3.plot(date, dppkerg3plt[:]+nppkerg3plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(dppkerg3plt[:]+nppkerg3plt[:]),max(dppkerg3plt[:]+nppkerg3plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('Prod',fontsize='large')
    figure.autofmt_xdate() # should be adapted a bit more...

    ax1 = figure.add_subplot(4,3,10)
    ax1.plot(date, dppkerg4plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(dppkerg4plt[:]),max(dppkerg4plt[:])])
    ax1.grid(True)
    plt.ylabel('Winter max MLD Box (20-120E 40-48S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('DProd',fontsize='large')
    ax2 = figure.add_subplot(4,3,11)
    ax2.plot(date, nppkerg4plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(nppkerg4plt[:]),max(nppkerg4plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'NProd',fontsize='large')
    else :
          plt.title('NProd',fontsize='large')
    ax3 = figure.add_subplot(4,3,12)
    ax3.plot(date, dppkerg4plt[:]+nppkerg4plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(dppkerg4plt[:]+nppkerg4plt[:]),max(dppkerg4plt[:]+nppkerg4plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('Prod',fontsize='large')
    figure.autofmt_xdate() # should be adapted a bit more...
    

    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_1m_ppkerg.png')

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
      fig.savefig('./ppkerg.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

