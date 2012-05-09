#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""__readtools__.py : A set of utilities for reading and manipulating 
                      netcdf and mtl files created by monitor_prod.ksh
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

import pydmontools as pydmt 
from pydmontools import CDF
import sys,os
import numpy
import matplotlib.dates as mdates


osp = os.sep

#=======================================================================

strmth2strnum = {'JAN':'01','FEB':'02','MAR':'03','APR':'04','MAY':'05','JUN':'06','JUL':'07',\
		'AUG':'08','SEP':'09','OCT':'10','NOV':'11','DEC':'12'}

def get_datetime(ncfile,tname='time_counter'):
    """Return a datetime.datetime object built from ncfile time_counter.
    Assume that time_counter is given in years. 
    """
    fid   = CDF.NetCDFFile(ncfile,'r')
    time_counter =  fid.variables[tname][:].squeeze()
    time_origin = fid.variables[tname].time_origin
    fid.close()
    try:
       yrs = int(time_origin[:4])
       mth = int(strmth2strnum[time_origin[5:8]])
       day = int(time_origin[9:11])
       hours = int(time_origin[12:14])
       minutes = int(time_origin[15:17])
       seconds = int(time_origin[18:20])
       date_origin = mdates.datetime.datetime(yrs,mth,day,hours,minutes,seconds)
       num_origin = mdates.date2num(date_origin)
    except:
       num_origin = 0 
       print 'time_origin attribute has not the expected format'
    _dates = mdates.num2date(time_counter/mdates.SECONDS_PER_DAY + num_origin) # num are given in days
    cleandate = lambda d:d.replace(hour=0,minute=0,second=0)
    dates = map(cleandate,_dates)
    return dates

def datafileroot(argdict): # could be used in individual plotting scripts...
    return argdict['datadir'] + osp + argdict['config'] + '-' + argdict['case']

def readfilenc(file,varname):
    fid   = CDF.NetCDFFile(file,'r')
    value = numpy.array(fid.variables[varname][:]).squeeze()
    fid.close()
    return value

def readfilenc0d(file,varname):
    fid   = CDF.NetCDFFile(file,'r')
    value = numpy.array(fid.variables[varname]).squeeze()
    fid.close()
    return value

def get_years(file,offset=0):
    fid   = CDF.NetCDFFile(file,'r')
    years = numpy.array(fid.variables['time_counter'][:]) 
    years = years + offset
    return years 

def get_years_intpart(file,offset=0):
    fid   = CDF.NetCDFFile(file,'r')
    years = numpy.array(fid.variables['time_counter'][:]) 
    years = numpy.floor(years) + offset
    return years 

def define_sections(argdict, file_section='drakkar_sections_table.txt'):
    if argdict['config'] == '':
        print 'The script needs to know the config name in order to create its section list'
        print 'diag mode -> use : script.py --config CONFIG' 
        print 'prod mode -> check your environment variables'
        sys.exit()
    else:
        pass
    #
    if os.path.isfile(argdict['datadir'] + osp + file_section):
        print 'drakkar_sections_table.txt taken from datadir'
        fid_section = open(argdict['datadir'] + osp + file_section,'r')
    elif os.path.isfile(file_section):
        print 'drakkar_sections_table.txt taken from current dir'
        fid_section = open(file_section,'r')
    else:
        print 'diag mode : drakkar_sections_table.txt must be in your current directory'
        print 'prod mode : drakkar_sections_table.txt is not in your datadir'
        sys.exit()
    #
    lines=[lines for lines in fid_section.readlines() if lines.find('#') != 0 ] # remove empty lines
    fid_section.close()
    truenames  = [line.split()[0] for line in lines if line.find(' ' + argdict['config'] + ' ') != -1 ]
    shortnames = [] ; longnames = [] ; sens = []
    #
    for kk in range(len(truenames)):
        describing_line = [ line for line in lines if line.find(truenames[kk]) != -1 ][0]
        elements = describing_line.split()
        shortnames.append(elements[1])
        longnames.append(elements[2])
        sens.append(elements[3])
    sens = numpy.array((sens),'i')
    return truenames, shortnames, longnames, sens

def define_trpsig(argdict, file_section='drakkar_trpsig_table.txt'):
    if argdict['config'] == '':
        print 'The script needs to know the config name in order to create its section list'
        print 'diag mode -> use : script.py --config CONFIG' 
        print 'prod mode -> check your environment variables'
        sys.exit()
    else:
        pass
    #
    if os.path.isfile(argdict['datadir'] + osp + file_section):
        print 'drakkar_trpsig_table.txt taken from datadir'
        fid_section = open(argdict['datadir'] + osp + file_section,'r')
    elif os.path.isfile(file_section):
        print 'drakkar_trpsig_table.txt taken from current dir'
        fid_section = open(file_section,'r')
    else:
        print 'diag mode : drakkar_trpsig_table.txt must be in your current directory'
        print 'prod mode : drakkar_trpsig_table.txt is not in your datadir'
        sys.exit()
    #
    lines=[lines for lines in fid_section.readlines() if lines.find('#') != 0 ] # remove empty lines
    fid_section.close()
    truenames  = [line.split()[0] for line in lines if line.find(' ' + argdict['config'] + ' ') != -1 ]
    shortnames = [] ; longnames = [] 
    #
    for kk in range(len(truenames)):
        describing_line = [ line for line in lines if line.find(truenames[kk]) != -1 ][0]
        elements = describing_line.split()
        shortnames.append(elements[1])
        longnames.append(elements[2])

    return truenames, shortnames, longnames

def find_closest_level(depth_array, mydepth):
    level=(numpy.abs(depth_array-mydepth)).argmin()
    return level

def nc_add_spval(tableau, valeur):
    masque = numpy.equal(tableau, valeur)
    out = numpy.ma.array(tableau, mask=masque)
    return out

def remove_spval(array, spval, newval):
    maskzeros = numpy.greater(array, spval)
    numpy.putmask(array, maskzeros, newval)
    return array

def mtl_flush(filename):
    f=open(filename,'r')
    lines=[lines for lines in f.readlines() if lines.strip() ] # remove empty lines
    f.close()
    return lines

def get_index(array, myindex):
	minidx = None 
	for i,v in enumerate(array): 
		if v < myindex: 
			minidx = i+1 
	return minidx

def mean_0(tab):
	import numpy
	b=0
	for i in range(0,len(tab),1):
		if tab[i]!=0:
			b+=1
	if b==0:
		moy=0
	else:
		moy=sum(tab)/b
	return moy

def getIndex(tab,val):
	from numpy import array
	b=1e35
	for j in range(0,len(tab),1):
		if tab[j]==val:
			b=j
	return b
