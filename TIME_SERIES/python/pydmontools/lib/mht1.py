#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""mht1 : meridionnal heat transport for drakkar runs
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

plot_name = 'mht1'

#=======================================================================
#--- Reading the data 
#=======================================================================

def read(argdict=myargs,fromfile=[]):
    #
    if fromfile!=[]:                      # diagnostic mode...
       if fromfile[0].endswith('.nc'):    # if ncfile name is provided
          if not(len(fromfile)==1):
             print 'please provide one netcdf filename'
             sys.exit() 
          return _readnc(fromfile[0], argdict=argdict) 
       elif fromfile[0].endswith('.mtl'): # if mtlfile name is provided
          if not(len(fromfile)==1):
             print 'please provide one mlt filename'
             sys.exit() 
          return _readmtl(fromfile[0], argdict=argdict)
       else:                               
          pass
    elif fromfile==[]:                    # production mode 
       file_nc  = _get_ncname(argdict=argdict)
       file_mtl = _get_mtlnames(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(file_nc):
          return _readnc(file_nc, argdict=argdict) 
       # or try the mlt version   
       elif os.path.isfile(file_mtl):
          return _readmtl(file_mtl, argdict=argdict)
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_HEAT.nc' 
    return filename

def _get_mtlnames(argdict=myargs):
    filemtl = argdict['datadir'] + osp + argdict['config'] + '-' \
            + argdict['case'] + '_heat.mtl' 
    return filemtl
 
#=======================================================================

def _readnc(filenc=None,argdict=myargs):
    #
    if argdict['config'].find('ORCA') == 0:
        list_field = ['zomht_glo', 'zomht_atl', 'zomht_inp','hflx_glo' , 'hflx_atl' , 'hflx_inp']
    else:
        list_field = ['zomht_glo', 'hflx_glo']
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['year']      = rs.get_years_intpart(filenc)
    outdict['lat']       = rs.readfilenc(filenc, 'nav_lat')
    for field in list_field:
        temp           = rs.readfilenc(filenc,field)
        outdict[field] = rs.remove_spval(temp, 9999, 0) 
    return outdict # return the dictionnary of values 


def _readmtl(filemtl=None,argdict=myargs):
    #
    if argdict['config'].find('ORCA') == 0:
        list_field = ['zomht_glo', 'hflx_glo' , 'zomht_atl' , 'hflx_atl' , 'zomht_inp' , 'hflx_inp']
        nbvar  = 2 ; nbzone = 3
    else:
        list_field = ['zomht_glo', 'hflx_glo']
        nbvar  = 2 ; nbzone = 1
    #
    f=open(filemtl,'r')
    lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
    f.close()
    #
    yearx=[] ; lat=[] ; alltab=[]
    #
    for chaine in lignes[0:1] :
        element=chaine.split()
        for k in range(1,len(element)) :
            lat.append(float(element[k]))
    
    for chaine in lignes[1:] :
        element=chaine.split()
        yearx.append(float(element[0]))
        for k in range(1,len(element)) :
            alltab.append(float(element[k]))

    alltab=npy.reshape(alltab, (-1,len(lat)))
    yearx=npy.reshape(yearx, (-1,nbvar*nbzone))
    year=yearx[:,0].copy()
    #
    outdict = {}
    outdict['year'] = year
    outdict['lat']  = lat
    for kk in range(nbzone*nbvar):
        outdict[list_field[kk]] = alltab[kk::(nbzone*nbvar),:].copy()

    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None,color='r',compare=False, **kwargs):
    #
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #
    if argdict['config'].find('ORCA') == 0:
        list_field = ['zomht_glo', 'hflx_glo' , 'zomht_atl' , 'hflx_atl' , 'zomht_inp' , 'hflx_inp']
        zone       = ['Global ', 'Atlantic ', 'Indo-Pacific ']
        nbvar  = 2 ; nbzone = 3
    else:
        list_field = ['zomht_glo', 'hflx_glo']
        zone       = ['Global ']
        nbvar  = 2 ; nbzone = 1
    #
    fig_size=[(float(nbvar) + 1) * 6., float(nbzone) * 5. ]
    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)
    #
    if compare:
        return figure
    #
    variable      = ['Southward Accumulated Qnet', 'Advective MHT']
    #
    for k in range(1,nbzone+1) :
        plt.subplot(nbzone,nbvar+1,3*(k-1)+1)
        plt.contourf(year, lat, npy.transpose(vars()[list_field[2*k-2]]))
        plt.colorbar(format='%.3f')
        plt.grid(True)
        plt.axis([min(year), max(year), min(lat), max(lat)])
        plt.ylabel(zone[k-1],fontsize='x-large')
        if k==1 :
            plt.title(variable[0],fontsize='large')
        plt.subplot(nbzone,nbvar+1,3*(k-1)+2)
        plt.contourf(year, lat, npy.transpose(vars()[list_field[2*k-1]]))
        plt.colorbar(format='%.3f')
        plt.grid(True)
        plt.axis([min(year), max(year), min(lat), max(lat)])
        if k==1 :
            if not(compare) :
                plt.title(argdict['config'] + '-' + argdict['case']+'\n'+variable[1],fontsize='large')
            else:
                plt.title(variable[1],fontsize='large')
        plt.subplot(nbzone,nbvar+1,3*(k-1)+3)
        plt.contourf(year, lat, npy.transpose(vars()[list_field[2*k-2]] - vars()[list_field[2*k-1]]))
        plt.colorbar(format='%.3f')
        plt.grid(True)
        plt.axis([min(year), max(year), min(lat), max(lat)])
        if k==1 :
            plt.title('Second minus first',fontsize='large')
 
    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_mht1.png')

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
      fig.savefig('./mht1.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

