#!/usr/bin/python
#=======================================================================
#                        General Documentation

"""mldkerg : time series of mixed-layer depth in boxes near Kerguelen islands
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
plot_name = 'mldkerg'
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
    if argdict.has_key('compared_configs'):
       compare_different_configs = (len(argdict['compared_configs'])>1)
    else: 
       compare_different_configs = False 
    # get the section names corresponding to the config
    outdict = {} # creates the dictionnary which will contain the arrays 
    date = rs.get_datetime(filenc)
    mld1kerg1 =[] ; mld1kerg2=[] ; mld1kerg3 = []; mld1kerg4 = []
    mld2kerg1 =[] ; mld2kerg2=[] ; mld2kerg3 = []; mld2kerg4 = []
    mld3kerg1 =[] ; mld3kerg2=[] ; mld3kerg3 = []; mld3kerg4 = []
    mld1kerg1.append(rs.readfilenc(filenc, 'mean_somxl010_KERG1'))
    mld1kerg2.append(rs.readfilenc(filenc, 'mean_somxl010_KERG3'))
    mld1kerg3.append(rs.readfilenc(filenc, 'mean_somxl010_KERG5'))
    mld1kerg4.append(rs.readfilenc(filenc, 'mean_somxl010_KERG6'))
    mld2kerg1.append(rs.readfilenc(filenc, 'mean_somxl030_KERG1'))
    mld2kerg2.append(rs.readfilenc(filenc, 'mean_somxl030_KERG3'))
    mld2kerg3.append(rs.readfilenc(filenc, 'mean_somxl030_KERG5'))
    mld2kerg4.append(rs.readfilenc(filenc, 'mean_somxl030_KERG6'))
    mld3kerg1.append(rs.readfilenc(filenc, 'mean_somxlt02_KERG1'))
    mld3kerg2.append(rs.readfilenc(filenc, 'mean_somxlt02_KERG3'))
    mld3kerg3.append(rs.readfilenc(filenc, 'mean_somxlt02_KERG5'))
    mld3kerg4.append(rs.readfilenc(filenc, 'mean_somxlt02_KERG6'))

    mld1kerg1 = npy.reshape(mld1kerg1, len(date))
    mld1kerg2 = npy.reshape(mld1kerg2, len(date))
    mld1kerg3 = npy.reshape(mld1kerg3, len(date))
    mld1kerg4 = npy.reshape(mld1kerg4, len(date))
    mld2kerg1 = npy.reshape(mld2kerg1, len(date))
    mld2kerg2 = npy.reshape(mld2kerg2, len(date))
    mld2kerg3 = npy.reshape(mld2kerg3, len(date))
    mld2kerg4 = npy.reshape(mld2kerg4, len(date))
    mld3kerg1 = npy.reshape(mld3kerg1, len(date))
    mld3kerg2 = npy.reshape(mld3kerg2, len(date))
    mld3kerg3 = npy.reshape(mld3kerg3, len(date))
    mld3kerg4 = npy.reshape(mld3kerg4, len(date))

    big = npy.transpose(npy.ones(len(date)))

    outdict['date']    = date
    outdict['mld1kerg1plt'] = -1 * mld1kerg1 * big
    outdict['mld1kerg2plt'] = -1 * mld1kerg2 * big
    outdict['mld1kerg3plt'] = -1 * mld1kerg3 * big
    outdict['mld1kerg4plt'] = -1 * mld1kerg4 * big
    outdict['mld2kerg1plt'] = -1 * mld2kerg1 * big
    outdict['mld2kerg2plt'] = -1 * mld2kerg2 * big
    outdict['mld2kerg3plt'] = -1 * mld2kerg3 * big
    outdict['mld2kerg4plt'] = -1 * mld2kerg4 * big
    outdict['mld3kerg1plt'] = -1 * mld3kerg1 * big
    outdict['mld3kerg2plt'] = -1 * mld3kerg2 * big
    outdict['mld3kerg3plt'] = -1 * mld3kerg3 * big
    outdict['mld3kerg4plt'] = -1 * mld3kerg4 * big

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
    ax1.plot(date, mld1kerg1plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(mld1kerg1plt[:]),max(mld1kerg1plt[:])])
    ax1.grid(True)
    plt.ylabel('Kerfix box (68-69E 50-51S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('MLD rho=0.01',fontsize='large')
    ax2 = figure.add_subplot(4,3,2)
    ax2.plot(date, mld2kerg1plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(mld2kerg1plt[:]),max(mld2kerg1plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'MLD rho=0.02',fontsize='large')
    else :
          plt.title('MLD rho=0.02',fontsize='large')
    ax3 = figure.add_subplot(4,3,3)
    ax3.plot(date, mld3kerg1plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(mld3kerg1plt[:]),max(mld3kerg1plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('MLD temp=-0.2',fontsize='large')
    figure.autofmt_xdate() # should be adapted a bit more...
    
    ax1 = figure.add_subplot(4,3,4)
    ax1.plot(date, mld1kerg2plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(mld1kerg2plt[:]),max(mld1kerg2plt[:])])
    ax1.grid(True)
    plt.ylabel('Plume3 box (80-82E 50-52S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('MLD rho=0.01',fontsize='large')
    ax2 = figure.add_subplot(4,3,5)
    ax2.plot(date, mld2kerg2plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(mld2kerg2plt[:]),max(mld2kerg2plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'MLD rho=0.02',fontsize='large')
    else :
          plt.title('MLD rho=0.02',fontsize='large')
    ax3 = figure.add_subplot(4,3,6)
    ax3.plot(date, mld3kerg2plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(mld3kerg2plt[:]),max(mld3kerg2plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('MLD temp=-0.2',fontsize='large')
    figure.autofmt_xdate() # should be adapted a bit more...
  
    ax1 = figure.add_subplot(4,3,7)
    ax1.plot(date, mld1kerg3plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(mld1kerg3plt[:]),max(mld1kerg3plt[:])])
    ax1.grid(True)
    plt.ylabel('Summer max MLD Box (20-120E 52-60S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('MLD rho=0.01',fontsize='large')
    ax2 = figure.add_subplot(4,3,8)
    ax2.plot(date, mld2kerg3plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(mld2kerg3plt[:]),max(mld2kerg3plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'MLD rho=0.02',fontsize='large')
    else :
          plt.title('MLD rho=0.02',fontsize='large')
    ax3 = figure.add_subplot(4,3,9)
    ax3.plot(date, mld3kerg3plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(mld3kerg3plt[:]),max(mld3kerg3plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('MLD temp=-0.2',fontsize='large')
    figure.autofmt_xdate() # should be adapted a bit more...

    ax1 = figure.add_subplot(4,3,10)
    ax1.plot(date, mld1kerg4plt[:], color + '.-')
    ax1.axis([min(_date),max(_date),min(mld1kerg4plt[:]),max(mld1kerg4plt[:])])
    ax1.grid(True)
    plt.ylabel('Winter max MLD Box (20-120E 40-48S)',fontsize='small')
    ps.set_dateticks(ax1)
    plt.title('MLD rho=0.01',fontsize='large')
    ax2 = figure.add_subplot(4,3,11)
    ax2.plot(date, mld2kerg4plt[:], color + '.-')
    ax2.axis([min(date),max(date),min(mld2kerg4plt[:]),max(mld2kerg4plt[:])])
    ax2.grid(True)
    ps.set_dateticks(ax2)
    if not(compare) :
          plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'MLD rho=0.02',fontsize='large')
    else :
          plt.title('MLD rho=0.02',fontsize='large')
    ax3 = figure.add_subplot(4,3,12)
    ax3.plot(date, mld3kerg4plt[:], color + '.-')
    ax3.axis([min(date),max(date),min(mld3kerg4plt[:]),max(mld3kerg4plt[:])])
    plt.grid(True)
    ps.set_dateticks(ax3)
    plt.title('MLD temp=-0.2',fontsize='large')
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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_1m_mldkerg.png')

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
      fig.savefig('./mldkerg.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

