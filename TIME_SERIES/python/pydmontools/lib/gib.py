#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""gib : temperature and salinity profile of med waters outflow compared with levitus for drakkar runs
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

plot_name = 'gib'
fig_size =  [15.,18.]
plt.rcParams.update({'figure.figsize': fig_size})
depthanom = 1200.
maxdepth  = 5000. # for plots
smin      = 34.5  # for plots
ncontour  = 75

#=======================================================================
#--- Reading the data 
#=======================================================================

def read(argdict=myargs,fromfiles=[]):
    if fromfiles!=[]:                      # diagnostic mode...
       if fromfiles[0].endswith('.nc'):    # if ncfile name is provided
          if not(len(fromfiles)==2):
             print 'please provide two nc filenames'
             sys.exit() 
          return _readnc(fromfiles[0], fromfiles[1]) 
       elif fromfiles[0].endswith('.mtl'): # if mtlfile name is provided
          print 'mtl files are no longer supported'
          sys.exit() 
       else:                               
          pass
    elif fromfiles==[]:                    #  production mode...
       filenc, levitus = _get_ncname(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(filenc) and os.path.isfile(levitus):
          return _readnc(filenc, levitus) 
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_TSGIB.nc' 
    filelevnc = argdict['datadir'] + osp + 'LEVITUS_y0000_TSGIB.nc'
    return filename, filelevnc

#=======================================================================
 
def _readnc(filenc=None,levitus=None):
    # 
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['levels']   = rs.readfilenc(filenc,'gdept')           
    outdict['year']     = rs.get_years_intpart(filenc) 
    outdict['tmodel']   = rs.readfilenc(filenc,'mean_votemper')  #2D
    outdict['smodel']   = rs.readfilenc(filenc,'mean_vosaline')  
    outdict['tlev']     = rs.readfilenc(levitus,'mean_votemper') #1D
    outdict['slev']     = rs.readfilenc(levitus,'mean_vosaline')  
    return outdict # return the dictionnary of values 

def _readmtl(filesmtl=None):
    #
    datalist=['year','levels','tlev','slev','tmodel','smodel']
    # init 
    year = [] ; levels = [] ; tlev = [] ; slev =[] ; tmodel =[] ; smodel = []
    # open the file
    f=open(filesmtl,'r')
    lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
    f.close()
    # data manipulation
    for chaine in lignes[0:1] : # this line contains levels depth
        element=chaine.split()
        for k in range(1, len(element)) :
            vars()[datalist[1]].append((float(element[k])))

    for chaine in lignes[1:2] : # temperature levitus
        element=chaine.split()
        for k in range(1, len(element)) :
            vars()[datalist[2]].append(float(element[k]))

    for chaine in lignes[2:3] : # salinity levitus
        element=chaine.split()
        for k in range(1, len(element)) :
            vars()[datalist[3]].append(float(element[k]))

    for chaine in lignes[3::2] : # temperature in model
        element=chaine.split()
        vars()[datalist[0]].append(float(element[0]))
        for k in range(1, len(element)) :	
            vars()[datalist[4]].append(float(element[k]))
    
    for chaine in lignes[4::2] : # salinity in model
        element=chaine.split()
        for k in range(1, len(element)) :	
            vars()[datalist[5]].append(float(element[k]))
    
    tmodel=npy.reshape(tmodel, (-1,len(levels)) )
    smodel=npy.reshape(smodel, (-1,len(levels)) )

    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['levels']  = npy.array((levels),'f')
    outdict['year']    = npy.array((year),'f')
    outdict['tmodel']  = npy.array((tmodel),'f')
    outdict['smodel']  = npy.array((smodel),'f')
    outdict['tlev']    = npy.array((tlev),'f')
    outdict['slev']    = npy.array((slev),'f')
    
    return outdict 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs,figure=None,color='r',levels=None,year=None,tmodel=None,smodel=None,tlev=None,slev=None,compare=False):
    
    tmodel = rs.remove_spval(tmodel, 999. , 0.)
    smodel = rs.remove_spval(smodel, 999. , 0.)
    tlev   = rs.remove_spval(tlev  , 999. , 0.)
    slev   = rs.remove_spval(slev  , 999. , 0.)
    tlev2D = npy.ones((len(year),1,)) * tlev
    slev2D = npy.ones((len(year),1,)) * slev
    
    if figure is None: # by default create a new figure
          figure = plt.figure()
    #
    
    plt.subplot(3,2,1)
    plt.plot(tlev, levels, 'b', tmodel[-1,:], levels, color)
    plt.grid(True)
    plt.axis([min(min(tmodel[-1,:]),min(tlev)), max(max(tmodel[-1,:]),max(tlev)),maxdepth,min(levels)])
    plt.ylabel('Depth')
    if not(compare) :
        plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Last Tgib profile 14W-10W, 36N-40N (blue is Levitus)')
    else:
        plt.title('Last Tgib profile 14W-10W, 36N-40N (blue is Levitus)')
    
    plt.subplot(3,2,3)
    plt.plot(slev, levels, 'b', smodel[-1,:], levels, color)
    plt.grid(True)
    plt.axis([smin, max(max(smodel[-1,:]),max(slev)),maxdepth,min(levels)])
    plt.ylabel('Depth')
    plt.title('Last Sgib profile 14W-10W, 36N-40N (blue is Levitus)')
    
    levelanom = rs.find_closest_level(levels, depthanom)
    lvlm1 = levelanom
    
    plt.subplot(3,2,5)
    plt.plot(year, (tmodel[:,lvlm1]-tlev[lvlm1]), color+'.-')
    plt.grid(True)
    plt.axis([min(year), max(year), min(tmodel[:,lvlm1]-tlev[lvlm1]), max(tmodel[:,lvlm1]-tlev[lvlm1])])
    plt.ylabel('T anom (1000m)')
    plt.title('Temperature Anomaly around '+str(depthanom)+' m')
    
    plt.subplot(3,2,6)
    plt.plot(year, (smodel[:,lvlm1]-slev[lvlm1]), color+'.-')
    plt.grid(True)
    plt.axis([min(year), max(year), min(smodel[:,lvlm1]-slev[lvlm1]), max(smodel[:,lvlm1]-slev[lvlm1])])
    plt.ylabel('S anom (1000m)')
    plt.title('Salinity Anomaly around ' +str(depthanom)+' m')

    if not(compare) :
        # 2D hovmuller
        plt.subplot(3,2,2)
        plt.contourf(year, levels, npy.transpose(tmodel-tlev2D),ncontour)
        plt.colorbar()
        plt.grid(True)
        plt.axis([min(year), max(year),maxdepth,min(levels)])
        plt.title(argdict['config'] + '-' + argdict['case'])
        plt.ylabel('Temperature anomaly')

        # 2D hovmuller
        plt.subplot(3,2,4)
        plt.contourf(year, levels, npy.transpose(smodel-slev2D),ncontour)
        plt.colorbar()
        plt.grid(True)
        plt.axis([min(year), max(year),maxdepth,min(levels)])
        plt.ylabel('Salinity anomaly')
    
    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_gib.png')

#=======================================================================
#--- main 
#=======================================================================

def main(): 
   # get arguments
   parser = pydmontools.standard_pydmt_script_parser()
   (options, args) = parser.parse_args()
   argdict = myargs
   argdict.update(vars(options)) # updated with the command line options
   infiles = args                # default value is an empty list
   # read, plot and save   
   values = read(argdict=argdict,fromfiles=infiles)
   fig = plot(**values)
   if len(args)==0:
      save(argdict=argdict,figure=fig)
   else:
      fig.savefig('./gib.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

