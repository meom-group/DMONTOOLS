#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""icemonth : monitoring of ice volume, area & extent for drakkar runs
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

plot_name = 'icemonth'

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
             + argdict['case'] + '_ICEMONTH.nc' 
    return filename

def _get_mtlnames(argdict=myargs):
    filemtl  = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_icemonth.mtl' 
    return filemtl
 
#=======================================================================

def _readnc(filenc=None, argdict=myargs):
    #
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['year_model'] = rs.get_years(filenc)
    outdict['NVolume'   ] = rs.readfilenc(filenc, 'NVolume' )
    outdict['NArea'     ] = rs.readfilenc(filenc, 'NArea' )
    outdict['NExnsidc'  ] = rs.readfilenc(filenc, 'NExnsidc' )
    outdict['SVolume'   ] = rs.readfilenc(filenc, 'SVolume' )
    outdict['SArea'     ] = rs.readfilenc(filenc, 'SArea' )
    outdict['SExnsidc'  ] = rs.readfilenc(filenc, 'SExnsidc' )
    #
    return outdict # return the dictionnary of values 


def _readmtl(filemtl=None, argdict=myargs):
    #
    lignes = rs.mtl_flush(filemtl)
    datalist=['year','varc','vant','aarc','aant','earc','eant']
    nmonth=12
    #
    for k in datalist:
        exec(k+'=[]') # init to empty array
    #
    for chaine in lignes[3:] :
        element=chaine.split()
        if argdict['config'].find('ORCA') == 0 :
            for k in range(1,1+nmonth) :
                year.append(float(element[0]) + ((float(k)-0.5)/nmonth) )
                varc.append(float(element[k]))
            for k in range( 1+nmonth,1+(2*nmonth) ) :
                vant.append(float(element[k]))
            for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                aarc.append(float(element[k]))
            for k in range( 1+(3*nmonth),1+(4*nmonth) ) :
                aant.append(float(element[k]))
            for k in range( 1+(4*nmonth),1+(5*nmonth) ) :
                earc.append(float(element[k]))
            for k in range( 1+(5*nmonth),1+(6*nmonth) ) :
                eant.append(float(element[k]))
        #
        elif argdict['config'].find('PERIANT') == 0 :
            for k in range(1,1+nmonth) :
                year.append(float(element[0]) + ((float(k)-0.5)/nmonth) )
                vant.append(float(element[k]))
            for k in range( 1+nmonth,1+(2*nmonth) ) :
                aant.append(float(element[k]))
            for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                eant.append(float(element[k]))
    
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['year_model'] = year
    outdict['NVolume'   ] = varc
    outdict['NArea'     ] = aarc
    outdict['NExnsidc'  ] = earc
    outdict['SVolume'   ] = vant
    outdict['SArea'     ] = aant
    outdict['SExnsidc'  ] = eant

    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs):
    #
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #
    if argdict['config'].find('ORCA') != -1 :
        north = 1 ; south = 1 
        print "icemonth.py : use default values (for global)"
    elif argdict['config'].find('PERIANT') != -1 :
        north = 0 ; south = 1
        print "icemonth.py : use custom values for PERIANT configs"
    elif argdict['config'].find('NATL') != -1 :
        north = 1 ; south = 0
        print "icemonth.py : use custom values for NATL configs"
    else :
        print "icemonth.py : Your config is not supported..."
        print "The monitoring will try to use default values from Global Configuration"
    #
    nbzone = north + south
    nbplotline = 3
    fig_size = [float(nbplotline) * 6., float(nbzone) * 5.]
    if figure is None: # by default create a new figure
          figure = plt.figure(figsize=fig_size)

    #
    if north == 1 :
        plt.subplot(nbzone,nbplotline,1)
        plt.plot(year_model, NVolume, color)
        plt.axis([min(year_model), max(year_model), 0, 60000])
        plt.grid(True)
        plt.ylabel('Northern',fontsize='large')
        plt.title('Ice Volume (10**9 m**3)',fontsize='large')
        
        plt.subplot(nbzone,nbplotline,2)
        plt.plot(year_model, NArea, color)
        plt.grid(True)
        plt.axis([min(year_model), max(year_model), 0, 15000])
        plt.title('Ice Area (10**9 m**2)',fontsize='large')
        
        plt.subplot(nbzone,nbplotline,3)
        plt.plot(year_model, NExnsidc, color)
        plt.grid(True)
        plt.axis([min(year_model), max(year_model), 0, 20000])
        plt.title('Ice extent (10**9 m**2)',fontsize='large')
        
    if south == 1 :  
        plt.subplot(nbzone,nbplotline,north*nbplotline + 1)
        plt.plot(year_model, SVolume, color)
        plt.grid(True)
        plt.axis([min(year_model), max(year_model), 0, 20000])
        plt.ylabel('Southern',fontsize='large')
        plt.title('Ice Volume (10**9 m**3)',fontsize='large')

        plt.subplot(nbzone,nbplotline,north*nbplotline + 2)
        plt.plot(year_model, SArea, color)
        plt.grid(True)
        plt.axis([min(year_model), max(year_model), 0, 20000])
        plt.title('Ice Area (10**9 m**2)',fontsize='large')

        plt.subplot(nbzone,nbplotline,north*nbplotline + 3)
        plt.plot(year_model, SExnsidc, color)
        plt.grid(True)
        plt.axis([min(year_model), max(year_model), 0, 20000])
        plt.title('Ice extent (10**9 m**2)',fontsize='large')
     
    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_icemonth.png')

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
      fig.savefig('./icemonth.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

