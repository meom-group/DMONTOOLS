#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""pydmontools : Python Drakkar Monitoring tools - plotting timeseries
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
import os  

#- List of modules in the package:

_pydmt_plotting_scripts = ['cable','gib','transports1','tsmean','tsmean_lev','icemonth','icenoaa','icetrd',\
                    'icetrd_min','maxmoc','maxmoc40','mht1','nino','tao_profiles','tao_undercurrent','trpsig']

_pydmt_core_utilities = ["__readtools__.py", "__plottools__.py","monitor_compare.py"]

__all__ = _pydmt_plotting_scripts + _pydmt_core_utilities

#- Various tools
def _class2dic(myclass):
      """Return a dictbuilt from the class.
      """
      dic = {}
      for argument in dir(myclass):
          if not(argument.startswith('__')):
             dic[argument] = eval('myclass.' + argument)
      return dic


#- Handle shell variables
#
_shellvarname_for = {}
_shellvarname_for['config']       = 'MONPY_CONFIG'
_shellvarname_for['case']         = 'MONPY_CASE'
_shellvarname_for['datadir']      = 'MONPY_DATADIR'
_shellvarname_for['plotdir']      = 'MONPY_PLOTDIR'
_shellvarname_for['dataobsdir']   = 'MONPY_DATAOBSDIR'

def _try_and_load_from_shell(shellvarname):
      """Try to load a shell variable from its name. Return an empty string
      if the shell variale is not defined.
      """
      #try:
      #      exec("out = os.environ['" + shellvarname +"']")
      #except:
      #      exec("out  = ''")
      #return out
      return os.environ.get(shellvarname,'')

class _core_arguments:
   """This class initialize the value of the most basic arguments loaded from shell variables  
   """
   def __init__(self):
       for argument in ['datadir','plotdir','dataobsdir']:
             shellvarname = _shellvarname_for[argument]
             loaded_value =  _try_and_load_from_shell(shellvarname)
             exec("self." + argument + " = loaded_value")

class _default_arguments(_core_arguments): # defined as a subclass of the previous class
   """This class stores the default value for the std arguments of the (single) plotting functions 
   """
   def __init__(self):
       _core_arguments.__init__(self)
       for argument in ['config','case']:
             shellvarname = _shellvarname_for[argument]
             loaded_value =  _try_and_load_from_shell(shellvarname)
             exec("self." + argument + " = loaded_value")
 
default_arguments = _default_arguments()         # create a class holding the default values of the standard scripts argument 
default_argdict = _class2dic(default_arguments)  # create a dictionnary holding the default values of the std scripts arg.


# define the standart parser for pydmt individual plotting scripts
#
def standard_pydmt_script_parser():
   """Return the standard parser for individual pydmt plotting scripts.
   """
   from optparse import OptionParser
   myargs = default_argdict
   parser = OptionParser()
   parser.add_option("-c", "--config", dest="config",
                  help="model configuration", default=myargs['config'])
   parser.add_option("-r", "--case", dest="case",
                  help="model run", default=myargs['case'])
   parser.add_option("-d", "--datadir", dest="datadir",
                  help="config-case monitoring database directory", default=myargs['datadir'])
   parser.add_option("-o", "--dataobsdir", dest="dataobsdir",
                  help="observations database directory", default=myargs['dataobsdir'])
   parser.add_option("-p", "--plotdir", dest="plotdir",
                  help="config-case monitoring plots directory", default=myargs['plotdir'])
   return parser

# Check the availability of a netcdf interface
#
_possible_netcdf_interfaces = ['netCDF4','scipy.io.netcdf','Scientific.IO.NetCDF','pynetcdf','pupynere']
#_possible_netcdf_interfaces = ['scipy.io.netcdf','Scientific.IO.NetCDF','pynetcdf','pupynere']

found_an_interface = False
while not(found_an_interface):
   interface = _possible_netcdf_interfaces[0]
   try:
     exec('import ' + interface + ' as CDF')
     found_an_interface = True
   except:
     print "Interface "+ interface  + " is not available on this plateform"
     _possible_netcdf_interfaces.remove(interface)
     
# unfortunately the syntax is slightly different for pupynere,
# this need to be fixed
if CDF.__name__=='scipy.io.netcdf' or CDF.__name__=='pupynere':
   CDF.NetCDFFile = CDF.netcdf_file

if CDF.__name__=='netCDF4':
   CDF.NetCDFFile = CDF.Dataset

# ===== end file =====

