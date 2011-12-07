#!/usr/bin/env python
from distutils.core import setup
import os,shutil,user
import glob

#- Read in the package version and author fields from the Python
#  source code __package_version__.py file:

execfile(os.curdir+os.sep+'lib'+os.sep+'__package_version__.py')

#- List the files that should be executable scripts 
#

los = glob.glob('lib/*.py')
_los = glob.glob('lib/_*.py')
for file in _los:
   los.remove(file)

#- Call distutils.core.setup
#  see http://docs.python.org/distutils/setupscript.html

setup(name='pydmontools',
      version=version,
      author=author,
      url=url,
      description=description,
      packages = ['pydmontools'],
      package_dir = {'pydmontools': 'lib'},
      scripts = los,
      package_data={'pydmontools': ['data/*.nc']}
     )

#- Set-up the default .dmontools/ if needed
#
_homedir = user.home
_sep = os.sep
_dir_pydmt = _homedir + _sep + '.dmontools'

if not(os.path.exists(_dir_pydmt)):
   print 'create  ~/.dmontools/ directory'
   os.mkdir(_dir_pydmt)

for cfgfilename in ['rc_monitor_compare.py']: 
   if not(os.path.exists(_dir_pydmt + os.sep + cfgfilename)):
      print 'copy default ' + cfgfilename + ' to ~/.dmontools/'
      shutil.copy(os.curdir+os.sep+'cfg'+os.sep+cfgfilename,_dir_pydmt+os.sep+cfgfilename)

for sectionlist in ['drakkar_sections_table.txt','drakkar_trpsig_table.txt']:
   print 'copy ' + sectionlist + ' to ~/.dmontools/'
   shutil.copy(os.curdir+os.sep+'../../../MONITOR_PROD'+os.sep+sectionlist,_dir_pydmt+os.sep+sectionlist)

