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
          if not(len(fromfile)==1):
             print 'please provide one mlt filename'
             sys.exit() 
          return _readmtl(fromfile[0],argdict=argdict)
       else:                               
          pass
    elif fromfile==[]:                    # production mode 
       filenc = _get_ncname(argdict=argdict)
       filemtl = _get_mtlnames(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(filenc):
          return _readnc(filenc,argdict=argdict) 
       # or try the mlt version   
       elif os.path.isfile(filemtl):
          return _readmtl(filemtl,argdict=argdict)
          
def _get_ncname(argdict=myargs):
    fileroot = argdict['datadir'] + osp + argdict['config'] + '-' \
            + argdict['case'] 
    filename = fileroot + '_TRPSIG.nc'
    return filename

def _get_mtlnames(argdict=myargs):
    fileroot = argdict['datadir'] + osp + argdict['config'] + '-' \
            + argdict['case']  
    file  = fileroot + '_TRPSIG.mtl'
    return file

#=======================================================================

def _readnc(filenc=None,argdict=myargs):
    # get the section names corresponding to the config
    (truenames, shortnames, longnames) = rs.define_trpsig(argdict)
    #
    nsection=len(truenames)
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['year_model' ]  = rs.get_years(filenc)
    outdict['sigma_class']  = rs.readfilenc(filenc, 'sigma_class')[0,:]

    for kk in shortnames:
        vartmp='sigtrp_' + kk
        outdict[vartmp] = rs.readfilenc(filenc, 'sigtrp_' + kk) 

    return outdict # return the dictionnary of values 


def _readmtl(filemtl=None,argdict=myargs):
    lignes = rs.mtl_flush(filemtl)
    sigma_class=[] ; year_model = []
    # get the section names corresponding to the config
    (truenames, shortnames, longnames) = rs.define_trpsig(argdict)
    nsection=len(truenames) ; trplist = []
    #
    for k in shortnames:
        vartmp= 'sigtrp_' + k
        trplist= trplist + [vartmp]
        exec( vartmp + '=[]' )
    #
    for chaine in lignes[0:1] :
        element=chaine.split()
        for k in range(1,len(element)) :
            sigma_class.append(float(element[k]))
    for n in range(1,nsection+1) :
        for chaine in lignes[n::nsection] :
            element=chaine.split()
            if n == 1 :
                year_model.append(float(element[0]))
            for k in range(1,len(element)) :
                vars()[trplist[n-1]].append(float(element[k]))
    
    for n in range(1,nsection+1) :
        vars()[trplist[n-1]] = npy.reshape( vars()[trplist[n-1]] , (len(year_model),-1) )

    outdict = {}
    outdict['year_model']  = npy.array((year_model),'f')
    outdict['sigma_class'] = npy.array((sigma_class),'f')
    for k in trplist:
        outdict[k] = npy.array((vars()[k]),'f')

    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None,color='r',compare=False, **kwargs): 
      
    for key in kwargs:
        exec(key+'=kwargs[key]')
     
    (truenames, shortnames, longnames) = rs.define_trpsig(argdict)
    nsection = len(truenames)
    fig_size = [15., float(nsection) * 5.]
    
    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)
    
    year=year_model
    sigma=sigma_class 
    ncontour=70
    sigmadens = rs.get_index(sigma,27.8)

    for k in range(len(shortnames)):
        exec('trpsig=sigtrp_' + shortnames[k])
        exec('titleplot=longnames[k]')
        titleplot=titleplot.replace('_',' ')

        trpdense = trpsig.copy()
        trpdense[:,0:sigmadens-1] = 0.
        transport_total = sum(npy.transpose(trpdense))
        trpsig = ma.masked_equal(trpsig,0.)
        if not(compare):
           plt.subplot(nsection,2,2*k+1)
           plt.contourf(year, sigma,npy.transpose(trpsig),ncontour,cmap=cm.jet_r)
           plt.colorbar(format='%.1f')
           plt.contour(year, sigma,npy.transpose(trpsig),[0],colors='white')
           plt.plot(year, 27.8 * npy.ones(year.shape),'w-.')
           plt.grid(True)
           plt.axis([min(year), max(year), 29.2, 25.3])
           plt.ylabel('Sigma classes',fontsize='large')
           plt.title(titleplot + ' (Sv)',fontsize='large')
        plt.subplot(nsection,2,2*k+2)
        plt.plot(year, transport_total, color)
        plt.grid(True)
        plt.axis([min(year), max(year),
        min(transport_total),max(transport_total)])
        plt.ylabel('Transport of dense waters (> 27.8)',fontsize='large')

    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_trpsig.png')

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

