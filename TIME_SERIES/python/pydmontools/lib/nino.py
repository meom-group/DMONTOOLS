#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""nino : monitoring of el nino for drakkar runs
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

plot_name = 'nino'
fig_size =  [15.,18.]
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
       file_nc, file_obs  = _get_ncname(argdict=argdict)
       file_mtl,file_obs  = _get_mtlnames(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(file_nc) and os.path.isfile(file_obs):
          return _readnc(file_nc, file_obs, argdict=argdict) 
       # or try the mlt version   
       elif os.path.isfile(file_mtl) and os.path.isfile(file_obs):
          return _readmtl(file_mtl, file_obs, argdict=argdict)
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_NINO.nc' 
    fileobs  = argdict['dataobsdir'] + osp + 'data_obs_DRAKKAR.nc'
    return filename, fileobs

def _get_mtlnames(argdict=myargs):
    filemtl  = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_nino.mtl' 
    fileobs  = argdict['dataobsdir'] + osp + 'data_obs_DRAKKAR.nc'
    return filemtl , fileobs
 
#=======================================================================

def _readnc(filenc=None,fileobs=None,argdict=myargs):
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['year_model'  ]  = rs.get_years(filenc)
    outdict['NINO12_model']  = rs.readfilenc(filenc, 'votemper_NINO12' )
    outdict['NINO34_model']  = rs.readfilenc(filenc, 'votemper_NINO34' )
    outdict['NINO3_model' ]  = rs.readfilenc(filenc, 'votemper_NINO3'  )
    outdict['NINO4_model' ]  = rs.readfilenc(filenc, 'votemper_NINO4'  )
    #
    year_tmp  = rs.readfilenc(fileobs, 'YEAR_ELNINO')
    month_tmp = rs.readfilenc(fileobs, 'MONTH_ELNINO')
    outdict['year_obs'    ]  = year_tmp + ( month_tmp - 0.5) / 12  # middle of the month
    outdict['NINO12_obs'  ]  = rs.readfilenc(fileobs, 'NINO1+2' )
    outdict['NINO34_obs'  ]  = rs.readfilenc(fileobs, 'NINO3.4' )
    outdict['NINO3_obs'   ]  = rs.readfilenc(fileobs, 'NINO3'   )
    outdict['NINO4_obs'   ]  = rs.readfilenc(fileobs, 'NINO4'   )
    #
    year_tmp  = rs.readfilenc(fileobs, 'YEAR_SOI')
    month_tmp = rs.readfilenc(fileobs, 'MONTH_SOI')
    outdict['year_soi'    ]  = year_tmp + ( month_tmp - 0.5) / 12  # middle of the month
    outdict['SOI'         ]  = rs.readfilenc(fileobs, 'SOI')
    
    return outdict # return the dictionnary of values 


def _readmtl(filemtl=None, fileobs=None, argdict=myargs):
    #
    f1=open(filemtl,'r')
    lignes1=[lignes1 for lignes1 in f1.readlines() if lignes1.strip() ] # remove empty lines
    f1.close()
    #
    datalist=['year','month','nino1','unused1','nino2','unused2','nino3','unused3','nino4','unused4']
    #
    for k in datalist:
        exec(k+'=[]')
    #
    for chaine in lignes1 :
        element=chaine.split()
        vars()[datalist[0]].append(float(element[0]) + (float(element[1]) -0.5)/12 )
        for k in range(1,len(datalist)) :
            vars()[datalist[k]].append(float(element[k]))
	
    outdict = {}
    outdict['year_model'  ]  = year
    outdict['NINO12_model']  = nino1
    outdict['NINO34_model']  = nino4
    outdict['NINO3_model' ]  = nino2
    outdict['NINO4_model' ]  = nino3
    #
    year_tmp  = rs.readfilenc(fileobs, 'YEAR_ELNINO')
    month_tmp = rs.readfilenc(fileobs, 'MONTH_ELNINO')
    outdict['year_obs'    ]  = year_tmp + ( month_tmp - 0.5) / 12  # middle of the month
    outdict['NINO12_obs'  ]  = rs.readfilenc(fileobs, 'NINO1+2' )
    outdict['NINO34_obs'  ]  = rs.readfilenc(fileobs, 'NINO3.4' )
    outdict['NINO3_obs'   ]  = rs.readfilenc(fileobs, 'NINO3'   )
    outdict['NINO4_obs'   ]  = rs.readfilenc(fileobs, 'NINO4'   )
    #
    year_tmp  = rs.readfilenc(fileobs, 'YEAR_SOI')
    month_tmp = rs.readfilenc(fileobs, 'MONTH_SOI')
    outdict['year_soi'    ]  = year_tmp + ( month_tmp - 0.5) / 12  # middle of the month
    outdict['SOI'         ]  = rs.readfilenc(fileobs, 'SOI')

    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):
    #
    if figure is None: # by default create a new figure
        figure = plt.figure()
    #
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #
    plt.subplot(5,1,1)
    plt.plot(year_model, NINO12_model, color, year_obs, NINO12_obs, 'b')
    plt.axis([min(year_model),                        max(max(year_model),  max(year_obs)), 
              min(min(NINO12_model),min(NINO12_obs)), max(max(NINO12_model),max(NINO12_obs))])
    plt.grid(True)
    plt.ylabel('Nino1+2', fontsize='small')
    
    plt.subplot(5,1,2)
    plt.plot(year_model, NINO3_model, color, year_obs, NINO3_obs, 'b')
    plt.axis([min(year_model),                      max(max(year_model), max(year_obs)), 
              min(min(NINO3_model),min(NINO3_obs)), max(max(NINO3_model),max(NINO3_obs))])
    plt.grid(True)
    plt.ylabel('Nino3', fontsize='small')
    
    plt.subplot(5,1,3)
    plt.plot(year_model, NINO4_model, color, year_obs, NINO4_obs, 'b')
    plt.axis([min(year_model),                      max(max(year_model), max(year_obs)), 
              min(min(NINO4_model),min(NINO4_obs)), max(max(NINO4_model),max(NINO4_obs))])
    plt.grid(True)
    plt.ylabel('Nino4', fontsize='small')
    
    plt.subplot(5,1,4)
    plt.plot(year_model, NINO34_model, color, year_obs, NINO34_obs, 'b')
    plt.axis([min(year_model),                        max(max(year_model),  max(year_obs)), 
              min(min(NINO34_model),min(NINO34_obs)), max(max(NINO34_model),max(NINO34_obs))])
    plt.grid(True)
    plt.ylabel('Nino3.4', fontsize='small')
    
    plt.subplot(5,1,5)
    plt.plot(year_soi, SOI, 'b')
    plt.axis([min(year_model), max(max(year_soi),max(year_model)), min(SOI), max(SOI)])
    plt.grid(True)
    plt.ylabel('SO Index', fontsize='small')
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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_nino.png')

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
      fig.savefig('./nino.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

