#!/usr/bin/python
#=======================================================================
#                        General Documentation

"""nutkerg : time series of mixed-layer depth in boxes near Kerguelen islands
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
plot_name = 'nutkerg'
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
    outdict['no3kerg1plt'] = rs.readfilenc(filenc, 'mean_3DNO3_KERG1')
    outdict['no3kerg2plt'] = rs.readfilenc(filenc, 'mean_3DNO3_KERG3')
    outdict['no3kerg3plt'] = rs.readfilenc(filenc, 'mean_3DNO3_KERG5')
    outdict['no3kerg4plt'] = rs.readfilenc(filenc, 'mean_3DNO3_KERG6')
    outdict['po4kerg1plt'] = rs.readfilenc(filenc, 'mean_3DPO4_KERG1')
    outdict['po4kerg2plt'] = rs.readfilenc(filenc, 'mean_3DPO4_KERG3')
    outdict['po4kerg3plt'] = rs.readfilenc(filenc, 'mean_3DPO4_KERG5')
    outdict['po4kerg4plt'] = rs.readfilenc(filenc, 'mean_3DPO4_KERG6')
    outdict['sikerg1plt'] = rs.readfilenc(filenc, 'mean_3DSi_KERG1')
    outdict['sikerg2plt'] = rs.readfilenc(filenc, 'mean_3DSi_KERG3')
    outdict['sikerg3plt'] = rs.readfilenc(filenc, 'mean_3DSi_KERG5')
    outdict['sikerg4plt'] = rs.readfilenc(filenc, 'mean_3DSi_KERG6')
    outdict['ferkerg1plt'] = rs.readfilenc(filenc, 'mean_3DFer_KERG1')
    outdict['ferkerg2plt'] = rs.readfilenc(filenc, 'mean_3DFer_KERG3')
    outdict['ferkerg3plt'] = rs.readfilenc(filenc, 'mean_3DFer_KERG5')
    outdict['ferkerg4plt'] = rs.readfilenc(filenc, 'mean_3DFer_KERG6')

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
    
    ax1 = figure.add_subplot(4,4,1)
    ax1.plot(date, no3kerg1plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(no3kerg1plt[:]),max(no3kerg1plt[:])])
    ax1.grid(True)
    plt.ylabel('Kerfix box (68-69E 50-51S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('NO3',fontsize='large')
    ax2 = figure.add_subplot(4,4,2)
    ax2.plot(date, po4kerg1plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(po4kerg1plt[:]),max(po4kerg1plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'PO4',fontsize='large')
    else :
          plt.title('PO4',fontsize='large')
    ax3 = figure.add_subplot(4,4,3)
    ax3.plot(date, sikerg1plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(sikerg1plt[:]),max(sikerg1plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('Si',fontsize='large')
    ax4 = figure.add_subplot(4,4,4)
    ax4.plot(date, ferkerg1plt[:], color + '.-')
    ax4.axis([min(date),max(date),min(ferkerg1plt[:]),max(ferkerg1plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax4)
    plt.title('Fer',fontsize='large')
    figure.autofmt_xdate() # should be adapted a bit more...
    
    ax1 = figure.add_subplot(4,4,5)
    ax1.plot(date, no3kerg2plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(no3kerg2plt[:]),max(no3kerg2plt[:])])
    ax1.grid(True)
    plt.ylabel('Plume3 box (80-82E 50-52S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('NO3',fontsize='large')
    ax2 = figure.add_subplot(4,4,6)
    ax2.plot(date, po4kerg2plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(po4kerg2plt[:]),max(po4kerg2plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'PO4',fontsize='large')
    else :
          plt.title('PO4',fontsize='large')
    ax3 = figure.add_subplot(4,4,7)
    ax3.plot(date, sikerg2plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(sikerg2plt[:]),max(sikerg2plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('Si',fontsize='large')
    ax4 = figure.add_subplot(4,4,8)
    ax4.plot(date, ferkerg2plt[:], color + '.-')
    ax4.axis([min(date),max(date),min(ferkerg2plt[:]),max(ferkerg2plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax4)
    plt.title('Fer',fontsize='large')
    figure.autofmt_xdate() # should be adapted a bit more...
  
    ax1 = figure.add_subplot(4,4,9)
    ax1.plot(date, no3kerg3plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(no3kerg3plt[:]),max(no3kerg3plt[:])])
    ax1.grid(True)
    plt.ylabel('Summer max MLD Box (20-120E 52-60S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('NO3',fontsize='large')
    ax2 = figure.add_subplot(4,4,10)
    ax2.plot(date, po4kerg3plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(po4kerg3plt[:]),max(po4kerg3plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'PO4',fontsize='large')
    else :
          plt.title('PO4',fontsize='large')
    ax3 = figure.add_subplot(4,4,11)
    ax3.plot(date, sikerg3plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(sikerg3plt[:]),max(sikerg3plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('Si',fontsize='large')
    ax4 = figure.add_subplot(4,4,12)
    ax4.plot(date, ferkerg3plt[:], color + '.-')
    ax4.axis([min(date),max(date),min(ferkerg3plt[:]),max(ferkerg3plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax4)
    plt.title('Fer',fontsize='large')
    figure.autofmt_xdate() # should be adapted a bit more...

    ax1 = figure.add_subplot(4,4,13)
    ax1.plot(date, no3kerg4plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(no3kerg4plt[:]),max(no3kerg4plt[:])])
    ax1.grid(True)
    plt.ylabel('Winter max MLD Box (20-120E 40-48S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('NO3',fontsize='large')
    ax2 = figure.add_subplot(4,4,14)
    ax2.plot(date, po4kerg4plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(po4kerg4plt[:]),max(po4kerg4plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'PO4',fontsize='large')
    else :
          plt.title('PO4',fontsize='large')
    ax3 = figure.add_subplot(4,4,15)
    ax3.plot(date, sikerg4plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(sikerg4plt[:]),max(sikerg4plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('Si',fontsize='large')
    ax3 = figure.add_subplot(4,4,16)
    ax3.plot(date, ferkerg4plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(ferkerg4plt[:]),max(ferkerg4plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('Fer',fontsize='large')
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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_1m_nutkerg.png')

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
      fig.savefig('./nutkerg.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

