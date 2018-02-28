######################################################
#!/usr/bin/env python 
######################################################
# Python Drakkar Monitoring Tools configuration file. 
######################################################
#
# Caution : 
# --------
# The above information are used by the comparison 
# script monitor_compare.py, not by the individual 
# plotting scripts (eg. transport1.py) 
#
# Content 
# -------
# The user can provide the following machine-dependent
# information : 
#
#   datadir
#   plotdir
#   remote_machine
#   remote_username 
#   remote_basedir
#   plotnames
#   verbose
#
#######################################################
# 

#- required plots
plotnames = ['cable','gib','transports1','tsmeanlat','tslatn','tslats','tslateq','tsmean','tsmean_lev',\
             'icemonth','icenoaa','icetrd','icetrd_min','maxmoc','maxmoc40','mht1','nino','tao_profiles',\
             'tao_undercurrent','trpsig']

#- level of verbosity (so far only 0 or 1) 
verbose = 1

#- Local configuration
local_send = False

#  If the following lines are commented, the default
#  value for the three following parameters will be
#  set by __init__.py to the values specified by the
#  environment variables.  
datadir    =  '' 
dataobsdir =  '' 
plotdir    =  ''

#- Remote configuration
remote_send     = False
remote_machine  =  'ige-meom-drakkar.u-ga.fr'
remote_username = 'drakkar'
remote_basedir  = 'DRAKKAR/'

#######################################################
#END OF FILE #
#######################################################
