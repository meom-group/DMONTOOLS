#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""trpsig : monitoring of transports in sigma classes for drakkar runs
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
from numpy import ma
import pydmontools 
from pydmontools import __readtools__ as rs
from pydmontools import __plottools__ as ps
plt = ps.plt 
cm =plt.cm

myargs = pydmontools.default_argdict
osp = os.sep

#- parameters
plot_name = 'trpsig'

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
       # first try to open a netcdf file
       if os.path.isfile(filenc):
          return _readnc(filenc,argdict=argdict) 
          
def _get_ncname(argdict=myargs):
    #
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_1m_TRPSIG.nc'

    return filename


#=======================================================================

def _readnc(filenc=None,argdict=myargs):
    if argdict.has_key('compared_configs'):
       compare = True
    else:
       argdict['compared_configs'] = [ argdict['config'] ]
       compare = False
    # get the section names corresponding to the config
    section_dict = rs.define_all_sections(argdict,compare,'drakkar_trpsig_table.txt')
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['date' ]  = rs.get_datetime(filenc)
    outdict['sigma_class']  = rs.readfilenc(filenc, 'sigma_class')[0,:]


    list_truenames = section_dict.keys()
    list_truenames.sort()

    for section in list_truenames:

       shortname = section_dict[section]['shortname']  # temporary

       try:
           outdict['sigtrp_' + shortname] = rs.readfilenc(filenc, 'sigtrp' + '_' + shortname)
       except:
           outdict['sigtrp_' + shortname] = npy.zeros(( len(outdict['date']), len(outdict['sigma_class']) ))

    return outdict # return the dictionnary of values 


#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None,color='r',compare=False, **kwargs): 
      
    for key in kwargs:
        exec(key+'=kwargs[key]')
     
    section_dict = rs.define_sections(argdict,compare,'drakkar_trpsig_table.txt')
    nsection=len(section_dict.keys())
    fig_size = [15., float(nsection) * 5.]
    
    list_truenames = section_dict.keys()
    list_truenames.sort()

    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)
    
    _date = ps.mdates.date2num(date) # now a numerical value
    sigma=sigma_class 
    ncontour=70
    sigmadens = rs.get_index(sigma,27.8)

    pltline = 0 # counter for plot line

    nonsense = 9e15 # the greatest trick : for compare mode set min and max
                    # default values to non-sense so that they will be replaced by following plot


    for section in list_truenames:

        shortname = section_dict[section]['shortname']
        longname  = section_dict[section]['longname']

        exec('trpsig = ' + 'sigtrp_' + shortname)
        titleplot = longname
        titleplot = titleplot.replace('_',' ')

        trpdense = trpsig.copy()
        trpdense[:,0:sigmadens-1] = 0.
        transport_total = sum(npy.transpose(trpdense))
        trpsig = ma.masked_equal(trpsig,0.)
        if not(compare):
           ax1 = figure.add_subplot(nsection,2,2*pltline+1)
           plt.contourf(date, sigma,npy.transpose(trpsig),ncontour,cmap=cm.jet_r)
           plt.colorbar(format='%.1f')
           plt.contour(date, sigma,npy.transpose(trpsig),[0],colors='white')
           plt.plot(date, 27.8 * npy.ones(_date.shape),'w-.')
           ax1.grid(True)
           ax1.axis([min(_date), max(_date), 29.2, 25.3])
           plt.ylabel('Sigma classes',fontsize='large')
           plt.title(titleplot + ' (Sv)',fontsize='large')
	   ps.set_dateticks(ax1)
        # when comparison is done only next plot is done
        # and we want it on the left of the screen
        if not(compare) :
           ax2 = figure.add_subplot(nsection,2,2*pltline+2)
        else :
           ax2 = figure.add_subplot(nsection,2,2*pltline+1)

        titleplot=titleplot.replace('transport in sigma classes',' ')
        if not(compare) and pltline == 0 :
           plt.title(argdict['config'] + '-' + argdict['case']+'\n'+titleplot , fontsize='large')
        else:
           plt.title(titleplot , fontsize='large')

        ax2.axis([min(_date),max(_date),nonsense,-nonsense])
        if transport_total.mean() !=0:
           ax2.plot(date, transport_total, color)
           ax2.grid(True)
           ax2.axis([min(_date), max(_date),
           min(transport_total),max(transport_total)])
        plt.ylabel('Dense waters trp.(> 27.8) (Sv) ', fontsize='large')
        ps.set_dateticks(ax2)
        pltline = pltline + 1

    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_1m_trpsig.png')

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
   
   fig = plot(argdict=argdict, **values)
   if len(args)==0:
      save(argdict=argdict,figure=fig)
   else:
      fig.savefig('./trpsig.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

