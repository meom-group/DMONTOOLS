################################################################################################
### Create NOAA/NSIDC netcdf file from ftp
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
except:
        print 'your python must be dead...' ; exit()


################################################################################################
####################################### Get the files ##########################################
################################################################################################

def download_from_ftp():
	ct = 0
	for month in ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']:
		ct = ct + 1
		mm = str(ct).zfill(2)
		ftpserv = 'sidads.colorado.edu/'
		dirdata = 'pub/DATASETS/NOAA/G02135/'
		fice_south='S_' + mm + '_area.txt'
		fice_north='N_' + mm + '_area.txt'
		urllib.urlretrieve('ftp://' + ftpserv + dirdata + month + '/' + fice_south,filename=fice_south)
		urllib.urlretrieve('ftp://' + ftpserv + dirdata + month + '/' + fice_north,filename=fice_north)
	return None

################################################################################################
####################################### Sort the data ##########################################
################################################################################################

def get_data_onemonth(filename,region,index=26):
	# region is 'N' or 'S'
	# index is the position on the text line of characters N or S, used to pick data
	# we should find something better like checking that the 4 first caracters are a year,...
	f = open(filename,'r')
	alllines = [alllines for alllines in f.readlines() if (alllines.find(region) == index) ]
	f.close()
	return alllines

def get_data_onepole(region):
	# region is 'N' or 'S'
	linesorted = []
	for month in npy.arange(1,13):
		mm = str(month).zfill(2)
		tmp = get_data_onemonth(region + '_' + mm + '_area.txt',region)
		for line in tmp:
			linesorted.append( line )
	linesorted.sort()
	return linesorted

def reshape_data_onepole(region):
	# region is 'N' or 'S'
	year = [] ; month = [] ; extent = [] ; area = []
	lines = get_data_onepole(region)

	for line in lines:
		elts = line.split()
		year.append(   int(  elts[0]))
		month.append(  int(  elts[1]))
		extent.append( float(elts[4]))
		area.append(   float(elts[5]))

	oyear = npy.array(year)
	omonth = npy.array(month)
	oextent = npy.array(extent)
	oarea = npy.array(area)
	
	return oyear, omonth, oextent, oarea

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
def write_1d_timeserie(ncfile,time,Nextent,Narea,Sextent,Sarea):
        fid = Dataset(ncfile, 'w', format='NETCDF3_CLASSIC')
        fid.description = 'Ice extent/area from NOAA/NSIDC \n file created by DMONTOOLS (MEOM CNRS)'
        # dimensions
        fid.createDimension('time_counter', time.shape[0])
        # variables
        otime      = fid.createVariable('time_counter', 'f4', ('time_counter',))
        oNextent   = fid.createVariable('NORTH_ICE_EXTENT', 'f4', ('time_counter',))
        oNarea     = fid.createVariable('NORTH_ICE_AREA', 'f4', ('time_counter',))
        oSextent   = fid.createVariable('SOUTH_ICE_EXTENT', 'f4', ('time_counter',))
        oSarea     = fid.createVariable('SOUTH_ICE_AREA', 'f4', ('time_counter',))
        # time
        otime[:]          = time
	otime.units       = 'seconds since 0001-01-01 00:00:00'
	otime.calendar    = 'gregorian'
	otime.time_origin = '0001-JAN-01 00:00:00'
        # data
        oNextent[:]   = Nextent    ; oNextent.missing_value = -9999.
        oNarea[:]     = Narea      ; oNarea.missing_value   = -9999.
        oSextent[:]   = Sextent    ; oSextent.missing_value = -9999.
        oSarea[:]     = Sarea      ; oSarea.missing_value   = -9999.
	msqkm = 'million square kilometers (10e6 km2)'
	oNextent.units = msqkm ; oNarea.units = msqkm
	oSextent.units = msqkm ; oSarea.units = msqkm
        # close
        fid.close()
        return None


################################################################################################
####################################### Main ###################################################
################################################################################################

def main():

	# 1. download files from server
	download_from_ftp()

	# 2. read data in monthly txt files and reshape
	yearN, monthN, extentN, areaN = reshape_data_onepole('N')
	yearS, monthS, extentS, areaS = reshape_data_onepole('S')

	# 3. correction of north extent

	# 1) The "extent" column includes the area near the pole not imaged by the
	# sensor. It is assumed to be entirely ice covered with at least 15%
	# concentration. However, the "area" column excludes the area not imaged by
	# the sensor. This area is 1.19 million square kilometers for SMMR (from the
	# beginning of the series through June 1987) and 0.31 million square
	# kilometers for SSM/I (from July 1987 to present). Therefore, there is a
	# discontinuity in the "area" data values in this file at the June/July 1987
	# boundary.

	ind_disc = (npy.abs(yearN - 1987)).argmin() + 6 # july 1987

	areaN[:ind_disc] = areaN[:ind_disc] + 1.19
	areaN[ind_disc:] = areaN[ind_disc:] + 0.31
	
	# correction change spval
	areaN[npy.where(areaN < 0)] = -9999.

	# 4. Setup of time axis
	timeN = yearmonth_to_seconds(yearN,monthN)
	#timeS = yearmonth_to_seconds(yearS,monthS) # useless 

	# 5. Write data to netcdf file
	write_1d_timeserie('./NC/dmondata_ice_NOAA.nc',timeN,extentN,areaN,extentS,areaS)
	
	# 6. Clean the txt files
	subprocess.call("rm *area.txt", shell=True)
	return None

if __name__ == '__main__':
	sys.exit(main())

