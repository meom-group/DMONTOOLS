#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""example.py : use this example template for creating your own diagnostics.

 
  This python script can be used in three distinct modes : 
 
  - in 'production' mode : the script is called without any argument. The 
    arguments are defined as shell variables (see e.g. run_monitor_py)

  - in 'testing' mode : any default argument handled by the standard_pydmt_script_parser  
    can be overriden by passing argument to the script (./example.py --datadir mydatadir)
    In that case the script will create a figure in your local directory : ./example.png

  - in 'comparison' mode : as any python script, this script can be used as a module. 
    Likewise, monitor_compare.py uses the core utility of pydmt scripts (namely the functions
    read() and plot()) for producing run-intercomparison plots.  


  Important remarks for developpers
  --------------------------------
  The compatibility with monitor_compare.py imposes some requirements on the structure 
  of this script : 

  - this script should produce one and only one figure. If you want to produce several 
    figures from your ncfile, you should write separate scripts. 

  - it should contain a read() and a plot(). These functions should take precisely 
    the arguments being used in this example and return the same output too. 
    Actually, what read() returns is given back as an argument to plot(). For compacity, 
    we gathered the output in a single dictionnary called outdic. All the keys of this 
    dictionnary are then reloaded in as local variables of plot().)

  - any 2D plot should be inserted in a conditionnal if loop so that it will not be 
    plotted in comparison mode. Namely 
    {{{
    if compare:
       pylab.contourf(mydata)
    }}}


  when your script is ready, you can insclude it to pydm as follows :   
  - move it to ../lib/ 
  - add 'example.py' to the list _pydmt_plotting_scripts in ../lib/__init__.py 
  - reinstall pydmt with >python setup.py install 

"""
#-----------------------------------------------------------------------
#                       Additional Documentation
#
# Modification History:
#  - Sept 2010: Original module by R. Dussin and J. Le Sommer
#
# Copyright (c) 2010, R. Dussin and J. Le Sommer. For licensing, distribution
# conditions, contact information, and additional documentation see the wiki :
# https://servforge.legi.grenoble-inp.fr/projects/DMONTOOLS/wiki/TIME_SERIES/python.
#=======================================================================

#-----------------------------------------------------------------------

### This example takes two netcdf input files (filemodel and fileobs)
### assuming diag1 and obs1 are 1d (vertical profile) and diag2 and obs2
### are 2d (hovmuller)

#- Module General Import and Declarations

import sys,os                                # OS and system calls
import numpy as npy                          # numpy is for numeric computations
import pydmontools 
from pydmontools import __readtools__ as rs
from pydmontools import __plottools__ as ps
plt = ps.plt 

myargs = pydmontools.default_argdict         # default argument dictionnary for script
osp = os.sep

#- parameters

plot_name = 'example'

#=======================================================================
#--- Reading the data 
#=======================================================================

### This is a read function which allows to switch between diagnostic mode
### where the user uses the script in command line giving the files as arguments
### ( >>> example.py --options toto.nc tata.nc ) and production mode where the
### filenames are defined by the script according to drakkar nomenclature.

def read(argdict=myargs,fromfiles=[]):
    """Read the data.

    This function is a wrapper which calls the appropriate function depending 
    on how the script is used  (production mode, testing mode or comparison).

    Parameters
    ---------
    argdict : dictionnary of script arguments
    fromfile : list which contains the filenames to be read. If the list is 
               empty, get_ncname will be used for defining the file to be read.

    """
    if fromfiles!=[]:                                            ## we have arguments so it is diagnostic mode
       if fromfiles[0].endswith('.nc'):                          ## check files are in netcdf
          if not(len(fromfiles)==2):                             ## check correct number of input files, here 2
             print 'please provide two nc filenames'             ## error print
             sys.exit()                                          ## quit the script
          return _readnc(fromfiles[0], fromfiles[1])             ## call to main read file
       else:                               
          print 'file format non supported'                      ## error print
          sys.exit()                                             ## quit the script
    elif fromfiles==[]:                                          ## no arguments so it is production mode
       filemodel, fileobs = _get_ncname(argdict=argdict)         ## ask for the filenames
       if os.path.isfile(filemodel) and os.path.isfile(fileobs): ## verify that those files exists
          return _readnc(filemodel, fileobs)                     ## call to main read file
       else:
          print 'file not found'
          sys.exit()

### This function gives the filename according to drakkar nomenclature
### when we are in production mode
          
def _get_ncname(argdict=myargs):
    """Return the filenames expected by _readnc().

    The path to the files is defined from the script arguments argdict.
    """
    filemodel = argdict['datadir'] + osp + argdict['config'] + '-'    ## model files are stored in datadir
    filemodel+= argdict['case'] + '_MYDIAG.nc' 
    fileobs = argdict['dataobsdir'] + osp + 'my_obs_file.nc'            ## obs files should be stored in dataobsdir
    return filemodel, fileobs                                           ## to be shared

#=======================================================================

### This function reads the netcdf files and return a dictionnary
### containing all the arrays needed by the plot function
 
def _readnc(filemodel=None,fileobs=None):
    """Read the netcdf files and return a dictionnary of output. 

    Remark
    ------
    All the keys of outdict will be loaded as local variables in plot()
    """
    # 
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['levels']   = rs.readfilenc(filemodel,'gdept')                ## read "gdept" in netcdf file "filemodel"
    outdict['year']     = rs.get_years(filemodel)                         ## read time_counter in "filemodel"
    outdict['diag1']    = rs.readfilenc(filemodel,'diag1_name_in_file')   ## ...
    outdict['diag2']    = rs.readfilenc(filemodel,'diag2_name_in_file')  
    outdict['obs1']     = rs.readfilenc(fileobs,'obs1_name_in_file') 
    outdict['obs2']     = rs.readfilenc(fileobs,'obs2_name_in_file')  
    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

### This is the plot which actually does the plot from the
### outdic dictionnary returned by the read() function

def plot(argdict,figure=None,color='r',compare=False,**kwargs):
    """Plot the data and return a pylab.figure object.

    Parameters
    ----------
    figure : a pylab.figure object on which the data will be plotted. 
    color : string defining the color of the 1D  pylab.plots
    compare : boolean. if False, all the plots are plotted; If True
              only the 1D plots are plotted. 

    """ 
    if figure is None: # by default create a new figure
          figure = plt.figure()
    # 
    # call back as local variables all the keys of the output dictionnary
    # returned by the function read() (which have been passed as arguments)
    for key in kwargs: 
          exec(key+'=kwargs[key]')

    # This is a simple plot
    plt.subplot(2,2,1)
    plt.plot(diag1, levels, color, obs1, levels, '-b')
    plt.grid(True)
    plt.axis([min(obs1),max(obs1),min(levels),max(levels)])
    plt.xlabel('label en x')
    plt.ylabel('label en y')
    plt.title('titre')
    
    # This is a 2D plot which makes
    # a hovmuller of the difference diag minus obs
    if compare:  # 2D plots should not be plotted in comparison mode
       plt.subplot(2,2,2)
       plt.contourf(year, levels,diag2 - obs2, ncontour)
       plt.colorbar()
       plt.axis([min(year), max(year), min(levels), max(levels)])
       plt.xlabel('label en x')
       plt.ylabel('label en y')
       plt.title('titre')
    #
    return figure # Returning the figure is crucial for comparison plots

#=======================================================================
#--- saving the plot 
#=======================================================================

### This function saves the plot produced by the plot function
### to the plots directory (only in production mode)

def save(argdict=myargs,figure=None):
    """Save the figure (only used in production mode).

    The filename is build from the information of argdict.  
    """
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_' + plot_name+ '.png')

#=======================================================================
#--- main 
#=======================================================================

### nothing to touch below 

def main(): 
   """Main function : parse arguments, read the data, produce and save 
   the figure.
   """
   # get arguments
   parser = pydmontools.standard_pydmt_script_parser()
   (options, args) = parser.parse_args()
   argdict = myargs
   argdict.update(vars(options)) # updated with the command line options
   infiles = args                # default value is an empty list
   # read, plot and save   
   values = read(argdict=argdict,fromfiles=infiles)
   fig = plot(argdict,**values)
   if len(args)==0:              # if no argument is provided, use the std production output file
      save(argdict=argdict,figure=fig)
   else:                         # if arguments are provided, use a local output file
      fig.savefig('./' + plot_name + '.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

