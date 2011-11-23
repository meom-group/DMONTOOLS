#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""cable : monitoring of transports for drakkar runs
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

plot_name = 'cable'
fig_size =  [5.,5.]
plt.rcParams.update({'figure.figsize': fig_size})

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
          dummy, file_obs = _get_ncname(argdict=argdict)
          return _readnc(fromfile[0], file_obs, argdict=argdict) 
       elif fromfile[0].endswith('.mtl'): # if mtlfile name is provided
          if not(len(fromfile)==1):
             print 'please provide one mlt filename'
             sys.exit() 
          dummy, file_obs = _get_mtlnames(argdict=argdict)
          return _readmtl(fromfile[0], file_obs, argdict=argdict)
       else:                               
          pass
    elif fromfile==[]:                    # production mode 
       file_nc, file_obs   = _get_ncname(argdict=argdict)
       file_mtl,file_obs  = _get_mtlnames(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(file_nc) and os.path.isfile(file_obs):
          return _readnc(file_nc, file_obs, argdict=argdict) 
       # or try the mlt version   
       elif os.path.isfile(file_mtl) and os.path.isfile(file_obs):
          return _readmtl(file_mtl, file_obs, argdict=argdict)
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_TRANSPORTS.nc' 
    fileobs  = argdict['dataobsdir'] + osp + 'data_obs_DRAKKAR.nc'
    return filename, fileobs

def _get_mtlnames(argdict=myargs):
    filemtl = argdict['datadir'] + osp + argdict['config'] + '-' \
            + argdict['case'] + '_matrix.mtl' 
    fileobs  = argdict['dataobsdir'] + osp + 'data_obs_DRAKKAR.nc'
    return filemtl , fileobs
 
#=======================================================================

def _readnc(filenc=None,fileobs=None,argdict=myargs):
    # get the section names corresponding to the config
    (truenames, shortnames, longnames, sens) = rs.define_sections(argdict)
    # test on existence of florida bahamas section in list
    found = None
    for k in range(len(truenames)):
        if truenames[k].find('FLORIDA_BAHAMAS') >= 0:
            found = 1
    if found is None:
        print "Your domain do not contain the Florida Bahamas section"
        sys.exit()    
    else:
        pass
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['yearmodel'] = rs.get_years_intpart(filenc)
    outdict['trpmodel']  = -1 * rs.readfilenc(filenc, 'vtrp_floba' )
    outdict['yearobs']   = rs.readfilenc(fileobs, 'YEAR_CABLE')
    outdict['trpobs']    = rs.readfilenc(fileobs, 'CABLE')

    return outdict # return the dictionnary of values 


def _readmtl(filemtl=None,fileobs=None,argdict=myargs):
    #
    # get the section names corresponding to the config
    (truenames, shortnames, longnames, sens) = rs.define_sections(argdict)
    nsection=len(truenames)
    #
    # getting the index of the section
    for k in range(len(truenames)):
        if truenames[k].find('FLORIDA_BAHAMAS') >= 0:
            indsection = k

    f1=open(filemtl,'r')
    lignes1=[lignes1 for lignes1 in f1.readlines() if lignes1.strip() ] # remove empty lines
    f1.close()
    # 
    yearmodel = [] ; vtrp = [] ; yearobs = [] ; trpobs = []
    #
    for chaine in lignes1[3:] :
        element=chaine.split()
        yearmodel.append(float(element[0]))
        for k in range(1,1+nsection) :
            vtrp.append(float(element[k]))

    vtrp=npy.reshape(vtrp, (-1,nsection))

    outdict = {}
    outdict['yearmodel'] = yearmodel
    outdict['trpmodel']  = -1 * vtrp[:,indsection]
    outdict['yearobs']   = rs.readfilenc(fileobs, 'YEAR_CABLE')
    outdict['trpobs']    = rs.readfilenc(fileobs, 'CABLE')

    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None,color='r',yearmodel=None,trpmodel=None,yearobs=None,trpobs=None,compare=False):
    #
    if figure is None :
        figure = plt.figure()
    plt.plot(yearmodel,trpmodel,color + '.-',yearobs,trpobs,'b.-')
    plt.axis([min(yearmodel),max(max(yearmodel),max(yearobs)),min(min(trpmodel),min(trpobs)), 
              max(max(trpmodel),max(trpobs))])
    plt.grid(True)
    if not(compare) :
         plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Mass Transport - Obs (b)',fontsize='small')
    else:
         plt.title('Mass Transport - Obs (b)',fontsize='small')
    plt.ylabel('Transport cross cable section',fontsize='small')
    #
    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_cable.png')

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
      fig.savefig('./cable.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

