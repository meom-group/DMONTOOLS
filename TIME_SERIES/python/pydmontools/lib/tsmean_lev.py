#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""tsmean_lev : domain average temperature and salinity budgets compared with levitus for drakkar runs
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

plot_name = 'tsmean_lev'
fig_size =  [15.,18.]
plt.rcParams.update({'figure.figsize': fig_size})

#=======================================================================
#--- Reading the data 
#=======================================================================

def read(argdict=myargs,fromfiles=[]):
    if fromfiles!=[]:                      # diagnostic mode...
       if fromfiles[0].endswith('.nc'):    # if ncfile name is provided
          if not(len(fromfiles)==2):
             print 'please provide two nc filenames'
             sys.exit() 
          return _readnc(fromfiles) 
       elif fromfiles[0].endswith('.mtl'): # if mtlfile name is provided
          if not(len(fromfiles)==2):
             print 'please provide two mlt filenames'
             sys.exit() 
          return _readmtl(fromfiles)
       else:                               
          pass
    elif fromfiles==[]:                    #  production mode...
       filesnc  = _get_ncname(argdict=argdict)
       filesmtl        = _get_mtlnames(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(filesnc[0]) and os.path.isfile(filesnc[1]):
          return _readnc(filesnc) 
       # or try the mlt version   
       elif os.path.isfile(filesmtl[0]) and os.path.isfile(filesmtl[1]):
          return _readmtl(filesmtl)
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_TSMEAN.nc' 
    filelevnc = argdict['datadir'] + osp + 'LEVITUS_y0000_TSMEAN.nc'
    return filename,filelevnc

def _get_mtlnames(argdict=myargs):
    fileroot = argdict['datadir'] + osp + argdict['config'] + '-' \
            + argdict['case']  
    filet = fileroot + '_TMEAN_lev.mtl'
    files = fileroot + '_SMEAN_lev.mtl'
    return filet,files

#=======================================================================
 
def _readnc(filesnc=None):
    filenc,levitus = filesnc
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['tmean']    = rs.readfilenc(filenc,'mean_3Dvotemper') 
    outdict['smean']    = rs.readfilenc(filenc,'mean_3Dvosaline') 
    outdict['sshmean']  = rs.readfilenc(filenc,'mean_3Dsossheig')  
    outdict['levels']   = rs.readfilenc(filenc,'gdept')           
    outdict['year']     = rs.get_years_intpart(filenc) 
    outdict['tmodel']   = rs.readfilenc(filenc,'mean_votemper')
    outdict['smodel']   = rs.readfilenc(filenc,'mean_vosaline')  
    outdict['tlev']     = rs.readfilenc(levitus,'mean_votemper')
    outdict['slev']     = rs.readfilenc(levitus,'mean_vosaline')  
    return outdict # return the dictionnary of values 

def _readmtl(filesmtl=None):
    # filesmtl : list of mtl filenames ['TMEAN','SMEAN']
    datalist1 = ['unused1','unused2','unused3','levels'] # line 2 in TMEAN.mtl and SMEAN.mtl
    datalist2 = ['year', 'sshmean', 'tmean', 'tmodel']   # following lines in TMEAN.mtl
    datalist3 = ['year2', 'sshmean2','smean', 'smodel']  # following lines in SMEAN.mtl
    datalistlev1 = ['unused4', 'unused5', 'tlevmean','tlev'] # line 3 in TMEAN_lev.mtl
    datalistlev2 = ['unused4', 'unused5', 'slevmean','slev'] # line 3 in SMEAN_lev.mtl
    mtlfile1, mtlfile2 = filesmtl
    #- read the file
    file1 = open(mtlfile1,'r')
    lignes1 = [lignes1 for lignes1 in file1.readlines() if lignes1.strip() ] # remove empty lines
    file1.close()
    file2 = open(mtlfile2,'r')
    lignes2 = [lignes2 for lignes2 in file2.readlines() if lignes2.strip() ] # remove empty lines
    file2.close()
    #- initialize arrays
    for dat in datalist1 :
      exec(dat + '= []')
    for dat in datalist2 :
      exec(dat + '= []')
    for dat in datalist3 :
      exec(dat + '= []')
    for dat in datalistlev1 :
      exec(dat + '= []')
    for dat in datalistlev2 :
      exec(dat + '= []')
    #- data manipulation
    for chaine in lignes1[2:3] :
       element=chaine.split()
       for k in range(datalist1.index('levels'), len(element)) : # get levels depth
               levels.append(+1*(float(element[k])))

    for chaine in lignes1[3:4] : # get levitus data
       element=chaine.split()
       for k in range(0,datalistlev1.index('tlev')) : # get single values before tlev
               vars()[datalistlev1[k]].append(float(element[k]))
       for k in range(datalistlev1.index('tlev'), len(element)) :
               tlev.append((float(element[k])))

    for chaine in lignes1[4:] :
       element=chaine.split()
       for k in range(0,datalist2.index('tmodel')) : # get single values before tmodel
               vars()[datalist2[k]].append(float(element[k]))
       for k in range(datalist2.index('tmodel'), len(element)) : # get tmodel in each level
               tmodel.append(float(element[k]))

    for chaine in lignes2[3:4] : # get levitus data
       element=chaine.split()
       for k in range(0,datalistlev2.index('slev')) : # get single values before slev
               vars()[datalistlev2[k]].append(float(element[k]))
       for k in range(datalistlev2.index('slev'), len(element)) :
               slev.append((float(element[k])))

    for chaine in lignes2[4:] :
       element=chaine.split()
       for k in range(0,datalist3.index('smodel')) : # get single values before smodel
               vars()[datalist3[k]].append(float(element[k]))
       for k in range(datalist3.index('smodel'), len(element)) : # get smodel in each level
               smodel.append(float(element[k]))

    tmodel = npy.reshape(tmodel,(npy.size(year),-1))
    smodel = npy.reshape(smodel,(npy.size(year),-1))

    lovar = ['tmean','smean','sshmean','levels','tmodel','smodel','year','tlev','slev'] 
    # creates the dictionnary which will contain the arrays
    outdict = dict(zip(lovar,map(eval,lovar))) 
    return outdict 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs,figure=None,color='r',tmean=None,smean=None,sshmean=None,levels=None,year=None,tmodel=None,smodel=None,tlev=None,slev=None,compare=False):
    
    if figure is None: # by default create a new figure
          figure = plt.figure()
    # 
    list3Dmean = ['tmean', 'smean', 'sshmean']
    titles3Dmean = ['3D Global T Mean', '3D Global S Mean', '3D Global SSH Mean']
    nb3Dmean=len(list3Dmean)

    for k in range(nb3Dmean) :
            plt.subplot(3,nb3Dmean,k+1)
            plt.plot(year, vars()[list3Dmean[k]], color + '.-')
            plt.grid(True)
            plt.axis([min(year), max(year),min(vars()[list3Dmean[k]]),max(vars()[list3Dmean[k]])])
            # add the config-name in the title of the midle panel
            if not(compare) and k == 1 :
                plt.title(argdict['config'] + '-' + argdict['case']+'\n'+titles3Dmean[k])
            else :
                plt.title(titles3Dmean[k])

    ### 2) 2D PLOTS
    ncontour=75
 
    plt.subplot(3,nb3Dmean,nb3Dmean+1)
    plt.semilogy((tmodel[-1,:]-tlev[:]),levels,color)
    plt.grid(True)
    plt.axis([min(tmodel[-1,:]-tlev[:]), max(tmodel[-1,:]-tlev[:]),max(levels),min(levels)])
    plt.ylabel('Log Depth')
    plt.title('Global change in T')

    plt.subplot(3,nb3Dmean,2*nb3Dmean+1)
    plt.semilogy((smodel[-1,:]-slev[:]),levels,color)
    plt.grid(True)
    plt.axis([min(smodel[-1,:]-slev[:]), max(smodel[-1,:]-slev[:]),max(levels),min(levels)])
    plt.ylabel('Log Depth')
    plt.title('Global change in S')

    if not(compare):
       plt.subplot(3,nb3Dmean,nb3Dmean+2)
       plt.contourf(year, levels, npy.transpose(tmodel[:,:]-tlev[:]),ncontour)
       plt.colorbar()
       plt.grid(True)
       plt.yscale('log')
       plt.axis([ min(year), max(year), max(levels),min(levels) ])
       plt.ylabel('Temperature anomaly')
    
       plt.subplot(3,nb3Dmean,2*nb3Dmean+2)
       plt.contourf(year, levels, npy.transpose(smodel[:,:]-slev[:]),ncontour)
       plt.colorbar()
       plt.grid(True)
       plt.yscale('log')
       plt.axis([ min(year), max(year), max(levels),min(levels) ])
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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_tsmean_lev.png')

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
      fig.savefig('./tsmean_lev.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

