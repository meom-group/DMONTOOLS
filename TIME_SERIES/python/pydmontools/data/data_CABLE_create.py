################################################################################################
### Create CABLE (florida current transport) netcdf file from NOAA/AOML
### R.Dussin (geekez malin) for DMONTOOLS


### Try import the modules
try:
	import urllib
except:
	print 'package urllib not available...' ; exit()
try:
	import numpy as npy
except:
	print 'package numpy not available...' ; exit()
try:
	from netCDF4 import Dataset
except:
	print 'package netCDF4 not available...' ; exit()
try:
	import matplotlib.dates as mdates
except:
	print 'package matplotlib not available...' ; exit()
try:
	import subprocess
	import sys
	import time
except:
        print 'your python must be dead...' ; exit()


################################################################################################
####################################### Get the files ##########################################
################################################################################################

def define_list_years():
	year_historical = npy.arange(1982,1999)
	thisyear        = int(time.strftime('%Y',time.localtime()))
	year_modern     = npy.arange(2000,thisyear+1)
	year_tot        = npy.concatenate((year_historical,year_modern))
	return year_tot
	

def download_from_web(years):
	serv    = 'http://www.aoml.noaa.gov/'
	dirdata = 'phod/floridacurrent/'
	for year in years:
		print 'downloading file for year', year
		fcable   = 'FC_cable_transport_' + str(year) + '.dat'
		urllib.urlretrieve(serv + dirdata + fcable,filename=fcable)
	return None

################################################################################################
####################################### Sort the data ##########################################
################################################################################################

def get_data_cable_oneyear(filename):
	# read the file
	f = open(filename,'r')
	alllines = [alllines for alllines in f.readlines() if (alllines.find('%') == -1) ]
	f.close()
	# init lists
	year      = [] ; month  = [] ; day = []
	transport = []
	#
	for line in alllines[1:]:
		elts = line.split()
		year.append(  int( elts[0]) )
		month.append( int( elts[1]) )
		day.append(   int( elts[2]) )
		if elts[3] == 'NaN':
			transport.append( -9999. )
		else:
			transport.append( float(elts[3]) )
	
	zyear      = npy.array(year     )
	zmonth     = npy.array(month    )
	zday       = npy.array(day      )
	ztransport = npy.array(transport)
	
	return zyear, zmonth, zday, ztransport

def get_data_cable_allyear(years):
	#
	year = [] ; month  = [] ; day = [] ; transport = []
	for kt in years:
		fcable = 'FC_cable_transport_' + str(kt) + '.dat'
		currentyear, currentmonth, currentday, currenttransport = get_data_cable_oneyear(fcable)
		year      = npy.concatenate((year,currentyear))
		month     = npy.concatenate((month,currentmonth))
		day       = npy.concatenate((day,currentday))
		transport = npy.concatenate((transport,currenttransport))
	return year, month, day, transport

def integrate_to_monthly(year_array,month_array,day_array,transport_array,years_to_perform):
	st = years_to_perform.shape[0] ; ct = 0
	year_out = npy.zeros((12*st),'i') ; month_out = npy.zeros((12*st),'i')
	transport_out = npy.zeros(12*st)
	for kt in years_to_perform:
		for mm in npy.arange(1,13):
			# we spot the indices for which kt=year and mm=month
			indyymm = npy.where( (year_array == kt) & (month_array == mm) )
			tmp_tr = transport_array[indyymm]
			# masking special value
			masque = npy.equal(tmp_tr, -9999.)
			tmp_tr_masked = npy.ma.array(data=tmp_tr,mask=masque)
			# monthly mean
			tmp_mean = tmp_tr_masked.mean()
			# fill arrays for this step
			year_out[ct]      = int(kt)
			month_out[ct]     = int(mm)
			transport_out[ct] = tmp_mean
			# get rid of NaN due to conversion masked to normal array
			if npy.isnan(transport_out[ct]):
				transport_out[ct] = -9999.
			# go to next frame
			ct = ct + 1
	return year_out, month_out, transport_out
	

################################################################################################
####################################### Time conversion ########################################
################################################################################################

def yearmonth_to_seconds(yearX,monthX):
	timeX = npy.zeros(len(yearX))
	for kt in range(len(yearX)):
		datestr = str(yearX[kt]) + '-' + str(monthX[kt]).zfill(2) + '-15'
		days_since_year1 = mdates.datestr2num(datestr)
		seconds_since_year1 = 86400 * days_since_year1
		timeX[kt] = seconds_since_year1
	return timeX

################################################################################################
####################################### NetCDF output ##########################################
################################################################################################

## Write 1d timeserie
def write_1d_timeserie_cable(ncfile,time,transport):
        fid = Dataset(ncfile, 'w', format='NETCDF3_CLASSIC')
        fid.description = 'Florida Current Transport aka Cable (NOAA/AOML) \n file created by DMONTOOLS (MEOM CNRS)'
        # dimensions
        fid.createDimension('time_counter', time.shape[0])
        # variables
        otime      = fid.createVariable('time_counter', 'f4', ('time_counter',))
        otransport = fid.createVariable('CABLE',        'f4', ('time_counter',))
        # time
        otime[:]          = time
	otime.units       = 'seconds since 0001-01-01 00:00:00'
	otime.calendar    = 'gregorian'
	otime.time_origin = '0001-JAN-01 00:00:00'
        # data
        otransport[:]  = transport   ; otransport.missing_value = -9999.

	units = 'Sverdrup (Sv)'
	otransport.units = units 
        # close
        fid.close()
        return None


################################################################################################
####################################### Main ###################################################
################################################################################################

def main():
	
	# 1. download files from server
	years = define_list_years()
	download_from_web(years)
	
	# 2. read data in txt files
	year, month, day, transport = get_data_cable_allyear(years)

	# 3. convert to monthly values
	year_integ, month_integ, transport_integ = integrate_to_monthly(year,month,day,transport,years)
	
	# 4. Setup of time axis
	time_cable = yearmonth_to_seconds(year_integ,month_integ)
	
	# 4. Write data to netcdf file
	write_1d_timeserie_cable('./NC/dmondata_cable_NOAA-AOML.nc',time_cable,transport_integ)
	
	# 5. Clean the txt files
	subprocess.call("rm FC_cable_transport_????.dat", shell=True)

	return None

if __name__ == '__main__':
	sys.exit(main())
