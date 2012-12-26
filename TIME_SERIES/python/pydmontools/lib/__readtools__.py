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
import sys,os, user
import numpy
import numpy.ma as ma
import matplotlib.dates as mdates


osp = os.sep

#=======================================================================
# Parser related

def get_list_of_freq():
	list_of_freq = ['1m','1y'] # likely to be completed
	return list_of_freq

def check_freq_arg(freqin):
	list_of_freq = get_list_of_freq()
	if list_of_freq.count(freqin) == 1:
		return True
	else:
		print 'Wrong input argument for frequency of monitoring' ; exit()
	#

#=======================================================================
# Date/Calendar functions

strmth2strnum = {'JAN':'01','FEB':'02','MAR':'03','APR':'04','MAY':'05','JUN':'06','JUL':'07',\
		'AUG':'08','SEP':'09','OCT':'10','NOV':'11','DEC':'12'}

def get_datetime(ncfile,tname='time_counter'):
    """Return a datetime.datetime object built from ncfile time_counter.
    Assume that time_counter is given in seconds since its time_origin. 
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
    cleandate = lambda d:d.replace(hour=0,minute=0,second=0,microsecond=0.)
    dates = map(cleandate,_dates)
    # correct to gregorian calendar
    dates = correct_dates_calendar(ncfile,dates,date_origin,tname)
    return dates

def correct_dates_calendar(ncfile,dates,date_origin,tname='time_counter'):
	fid   = CDF.NetCDFFile(ncfile,'r')
	try:
		calendar = fid.variables[tname].calendar
		if calendar == 'gregorian':
			corrected_dates = dates
			pass # no correction is needed
		elif calendar == 'noleap':
			# add the 'lost' leap days
			corrected_dates = []
			for date in dates:
				nld    = how_many_leapdays(date,date_origin)
				#print ' adding leap days : ', nld
				deltat = mdates.datetime.timedelta(days=nld)
				#print 'old date =', date
				date   = date + deltat
				corrected_dates.append(date)
				#print 'new date =', date
		elif calendar == '360d':
			# RD: not even tested cos no run available...
			# add the 'lost' leap days + 5 days each year
			corrected_dates = []
			for date in dates:
				nld    = how_many_leapdays(date,date_origin)
				n5d    = (date.year - date_origin.year) * 5
				ntot   = nld + n5d
				#print ' adding leap days : ', nld
				deltat = mdates.datetime.timedelta(days=ntot)
				#print 'old date =', date
				date   = date + deltat
				corrected_dates.append(date)
				#print 'new date =', date
		else:
			print 'Unknown calendar' ; exit()
	except:
		print 'Calendar attribute not found'
	fid.close()
	return corrected_dates
	
def how_many_leapdays(date,date_origin):
	ct = 0 # number of leap days
	# if our origin is after the 29/02 there is
	# no need to add the leap day of the first year (if any)
	if date_origin.month >= 3:
		fyear = date_origin.year + 1
	else:
		fyear = date_origin.year
	# if current date after the 29/02, we have to add
	# the leap day of current year
	if date.month >= 3:
		lyear = date.year + 1
	else:
		lyear = date.year

	for year in numpy.arange(fyear,lyear):
		if _is_leap_year(year):
			ct = ct + 1
		else:
			pass
	return ct

def _is_leap_year(year):
   """Return True if year is a leap year
   """
   if year % 400 == 0:
      return True
   elif year % 100 == 0:
      return False
   elif year % 4 == 0:
      return True
   else:
      return False


## Obsolete functions
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

#=======================================================================
# NetCDF I/O

def datafileroot(argdict): # could be used in individual plotting scripts...
    return argdict['datadir'] + osp + argdict['config'] + '-' + argdict['case']

def readfilenc(file,varname):
    fid   = CDF.NetCDFFile(file,'r')
    value = numpy.array(fid.variables[varname][:]).squeeze()
    if hasattr(fid.variables[varname],'missing_value'):
        value = ma.masked_equal(value,fid.variables[varname].missing_value)
    fid.close()
    return value

def readfilenc0d(file,varname):
    fid   = CDF.NetCDFFile(file,'r')
    value = numpy.array(fid.variables[varname]).squeeze()
    fid.close()
    return value


def define_all_sections(argdict,compare,file_section='drakkar_sections_table.txt'):
    #
    if compare:
        homedir = user.home
        if os.path.isfile(homedir + '/.dmontools/' + file_section):
           fid_section = open(homedir + '/.dmontools/' + file_section,'r')
        else:
           print '---------------------------------------------------------------------------------'
           print 'Compare mode !'
           print file_section , ' is not in your .dmontools directory, please update DMONTOOLS'
           print 'aborting...'
           print '---------------------------------------------------------------------------------'
           sys.exit()
    else:
       if os.path.isfile(argdict['datadir'] + osp + file_section):
           print file_section , ' taken from datadir'
           fid_section = open(argdict['datadir'] + osp + file_section,'r')
       elif os.path.isfile(file_section):
           print file_section , ' taken from current dir'
           fid_section = open(file_section,'r')
       else:
           print 'diag mode : ' , file_section , ' must be in your current directory'
           print 'prod mode : ' , file_section , ' is not in your datadir'
           sys.exit()
    #
    lines=[lines for lines in fid_section.readlines() if lines.find('#') != 0 ] # remove empty lines
    fid_section.close()
    tmp = []
    for line in lines:
        lst = line.split()
        if len(lst) > 0:
           tmp.append( lst[0] ) 

    truenames = list(set(tmp))
    #
    sections_dict = {}
    for section in truenames:
        describing_line = [ line for line in lines if line.find(section) != -1 ][0]
        elements = describing_line.split()
        this_section_dict = {}
        this_section_dict['truename']  = elements[0]
        this_section_dict['shortname'] = elements[1]
        this_section_dict['longname']  = elements[2]
        if len(elements) == 4:
           this_section_dict['sens']   = elements[3]
        sections_dict[this_section_dict['truename']] = this_section_dict

    return sections_dict


def define_sections(argdict, compare, file_section='drakkar_sections_table.txt'):
    if argdict['config'] == '':
        print 'The script needs to know the config name in order to create its section list'
        print 'diag mode -> use : script.py --config CONFIG' 
        print 'prod mode -> check your environment variables'
        sys.exit()
    else:
        pass
    #
    if compare:
        homedir = user.home
        if os.path.isfile(homedir + '/.dmontools/' + file_section):
           fid_section = open(homedir + '/.dmontools/' + file_section,'r')
        else:
           print '---------------------------------------------------------------------------------'
           print 'Compare mode !'
           print file_section , ' is not in your .dmontools directory, please update DMONTOOLS'
           print 'aborting...'
           print '---------------------------------------------------------------------------------'
           sys.exit()
    else:
       if os.path.isfile(argdict['datadir'] + osp + file_section):
           print file_section , ' taken from datadir'
           fid_section = open(argdict['datadir'] + osp + file_section,'r')
       elif os.path.isfile(file_section):
           print file_section , ' taken from current dir'
           fid_section = open(file_section,'r')
       else:
           print 'diag mode : ' , file_section , ' must be in your current directory'
           print 'prod mode : ' , file_section , ' is not in your datadir'
           sys.exit()

    #
    lines=[lines for lines in fid_section.readlines() if lines.find('#') != 0 ] # remove empty lines
    fid_section.close()
    truenames = set([])
    for config in argdict['compared_configs']:
        tmp = set([line.split()[0] for line in lines if line.find(' ' + config + ' ') != -1 ])
        truenames = truenames.union(tmp)
    #
    sections_dict = {}
    for section in truenames:
        describing_line = [ line for line in lines if line.find(section) != -1 ][0]
        elements = describing_line.split()
        this_section_dict = {}
        this_section_dict['truename']  = elements[0]
        this_section_dict['shortname'] = elements[1]
        this_section_dict['longname']  = elements[2]
        if len(elements) == 4:
           this_section_dict['sens']   = elements[3]
        sections_dict[this_section_dict['truename']] = this_section_dict

    return sections_dict

### for MTL - obsolete sooner or later
def define_sections_old(argdict, file_section='drakkar_sections_table.txt'):
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

### obsolete -- for MTL only
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

def get_month_indexes(tab,month):
	list_ind = []
	for kt in range(len(tab)):
		if tab[kt].month == month:
			list_ind.append( kt )
	out = numpy.array(list_ind)
	return out
	
def year_from_date(tab):
	out = tab.copy()
	for kt in range(len(tab)):
		out[kt] = tab[kt].year
	return out	
