#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""transports1 : monitoring of transports for drakkar runs
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
plot_name = 'transports1'
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
    filename = fileroot + '_TRANSPORTS.nc'
    return filename

def _get_mtlnames(argdict=myargs):
    fileroot = argdict['datadir'] + osp + argdict['config'] + '-' \
            + argdict['case']  
    file  = fileroot + '_matrix.mtl'
    return file

#=======================================================================

def _readnc(filenc=None,argdict=myargs):
    if argdict.has_key('compared_configs'):
       compare = True
    else: 
       argdict['compared_configs'] = [ argdict['config'] ]
       compare = False
    # get the section names corresponding to the config
    section_dict = rs.define_all_sections(argdict,compare)
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    year = rs.get_years_intpart(filenc)
    outdict['year']    = year

    list_truenames = section_dict.keys()
    list_truenames.sort()

    for section in list_truenames:

       shortname = section_dict[section]['shortname']  # temporary
       sens      = section_dict[section]['sens']       # variables

       try:
           outdict['mass_' + shortname] = npy.array((sens),'f') * rs.readfilenc(filenc, 'vtrp' + '_' + shortname)
           outdict['heat_' + shortname] = npy.array((sens),'f') * rs.readfilenc(filenc, 'htrp' + '_' + shortname)
           outdict['salt_' + shortname] = npy.array((sens),'f') * rs.readfilenc(filenc, 'strp' + '_' + shortname)
       except:
           outdict['mass_' + shortname] = npy.zeros((year.shape[0]))
           outdict['heat_' + shortname] = npy.zeros((year.shape[0]))
           outdict['salt_' + shortname] = npy.zeros((year.shape[0]))

    return outdict # return the dictionnary of values 


def _readmtl(filemtl=None,argdict=myargs):
    f=open(filemtl,'r')
    lignes=[lignes for lignes in f.readlines() if lignes.strip() ] # remove empty lines
    f.close()
    # 
    (truenames, shortnames, longnames, sens) = rs.define_sections_old(argdict)
    nsection=len(truenames)
    outdict = {} # creates the dictionnary which will contain the arrays 
    #
    mass = [] ; heat = [] ; salt = [] ; year = []
    #
    for chaine in lignes[3:] :
        element=chaine.split()
        year.append(float(element[0]))
        for k in range(1,1+nsection) :
                mass.append(float(element[k]))
        for k in range( 1+nsection,1+(2*nsection) ) :
                heat.append(float(element[k]))
        for k in range( 1+(2*nsection),1+(3*nsection) ) :
                salt.append(float(element[k]))

    bigsens = (sens * npy.ones((len(year),nsection)))

    mass=npy.reshape(mass, (-1,nsection))
    heat=npy.reshape(heat, (-1,nsection))
    salt=npy.reshape(salt, (-1,nsection))

    outdict['year']    = year
    outdict['massplt'] = npy.transpose( mass * bigsens )
    outdict['heatplt'] = npy.transpose( heat * bigsens )
    outdict['saltplt'] = npy.transpose( salt * bigsens )

    print 'this diag is not supported in MTL anymore'
    sys.exit()

    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):

    for key in kwargs:
        exec(key + '=kwargs[key]')

    # adjust the figure size 
    section_dict = rs.define_sections(argdict,compare)
    nsection=len(section_dict.keys())
    fig_size = [18., float(nsection) * 5.]

    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)

    list_truenames = section_dict.keys()
    list_truenames.sort()

    pltline = 0 # counter for plot line

    nonsense = 9e15 # the greatest trick : for compare mode set min and max
                    # default values to non-sense so that they will be replaced by following plot

    for section in list_truenames:

          shortname = section_dict[section]['shortname']
          longname  = section_dict[section]['longname']

          exec('massplt =' + 'mass_' + shortname )
          exec('heatplt =' + 'heat_' + shortname )
          exec('saltplt =' + 'salt_' + shortname )

          ### mass transport
          plt.subplot(nsection, 3, 3*pltline + 1)
          plt.axis([min(year),max(year),nonsense,-nonsense])
          if massplt.mean() != 0.:
              plt.plot(year, massplt, color + '.-')
              plt.axis([min(year),max(year),min(massplt),max(massplt)])
          plt.grid(True)
          plt.ylabel(longname.replace('_',' '),fontsize='small')
          if pltline==0 :
                plt.title('Mass Transport',fontsize='large')

          ### heat transport
          plt.subplot(nsection, 3, 3*pltline + 2)
          plt.axis([min(year),max(year),nonsense,-nonsense])
          if heatplt.mean() != 0.:
              plt.plot(year, heatplt, color + '.-')
              plt.axis([min(year),max(year),min(heatplt),max(heatplt)])
          plt.grid(True)

          if pltline==0:
                if not(compare):
                      plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Heat Transport',fontsize='large')
                else :
                      plt.title('Heat Transport',fontsize='large')

          ### salt transport
          plt.subplot(nsection, 3, 3*pltline + 3)
          plt.axis([min(year),max(year),nonsense,-nonsense])
          if saltplt.mean() != 0.:
              plt.plot(year, saltplt, color + '.-')
              plt.axis([min(year),max(year),min(saltplt),max(saltplt)])
          plt.grid(True)
          if pltline==0 :
                plt.title('Salt Transport',fontsize='large')

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
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_transports1.png')

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
      fig.savefig('./transports1.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

