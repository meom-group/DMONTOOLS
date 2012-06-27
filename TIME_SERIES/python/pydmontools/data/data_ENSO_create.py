################################################################################################
### Create ENSO netcdf file from climaate prediction center (NOAA)
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

def download_from_web():
	serv    = 'http://www.cpc.ncep.noaa.gov/'
	dirdata = 'data/indices/'
	fnino   = 'sstoi.indices'
	fsoi    = 'soi'
	urllib.urlretrieve(serv + dirdata + fnino,filename=fnino)
	urllib.urlretrieve(serv + dirdata + fsoi, filename=fsoi)
	return None

################################################################################################
####################################### Sort the data ##########################################
################################################################################################

def get_data_ninoboxes(filename='sstoi.indices'):
	# read the file
	f = open(filename,'r')
	alllines = f.readlines()
	f.close()
	# init lists
	year   = [] ; month  = []
	NINO12 = [] ; ANOM12 = []
	NINO3  = [] ; ANOM3  = []
	NINO4  = [] ; ANOM4  = []
	NINO34 = [] ; ANOM34 = []
	#
	for line in alllines[1:]:
		elts = line.split()
		year.append(   int(  elts[0]))
		month.append(  int(  elts[1]))
		NINO12.append( float(elts[2]))
		ANOM12.append( float(elts[3]))
		NINO3.append(  float(elts[4]))
		ANOM3.append(  float(elts[5]))
		NINO4.append(  float(elts[6]))
		ANOM4.append(  float(elts[7]))
		NINO34.append( float(elts[8]))
		ANOM34.append( float(elts[9]))
	
	zyear   = npy.array(year  )
	zmonth  = npy.array(month )
	zNINO12 = npy.array(NINO12)
	zANOM12 = npy.array(ANOM12)
	zNINO3  = npy.array(NINO3 )
	zANOM3  = npy.array(ANOM3 )
	zNINO4  = npy.array(NINO4 )
	zANOM4  = npy.array(ANOM4 )
	zNINO34 = npy.array(NINO34)
	zANOM34 = npy.array(ANOM34)
	
	return zyear, zmonth, zNINO12, zANOM12, zNINO3, zANOM3, zNINO4, zANOM4, zNINO34, zANOM34

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
def write_1d_timeserie_nino(ncfile,time,NINO12,ANOM12,NINO3,ANOM3,NINO4,ANOM4,NINO34,ANOM34):
        fid = Dataset(ncfile, 'w', format='NETCDF3_CLASSIC')
        fid.description = 'ENSO indices from Climate Prediction Center (NOAA) \n file created by DMONTOOLS (MEOM CNRS)'
        # dimensions
        fid.createDimension('time_counter', time.shape[0])
        # variables
        otime      = fid.createVariable('time_counter', 'f4', ('time_counter',))
        oNINO12    = fid.createVariable('NINO1+2',      'f4', ('time_counter',))
        oANOM12    = fid.createVariable('ANOM1+2',      'f4', ('time_counter',))
        oNINO3     = fid.createVariable('NINO3',        'f4', ('time_counter',))
        oANOM3     = fid.createVariable('ANOM3',        'f4', ('time_counter',))
        oNINO4     = fid.createVariable('NINO4',        'f4', ('time_counter',))
        oANOM4     = fid.createVariable('ANOM4',        'f4', ('time_counter',))
        oNINO34    = fid.createVariable('NINO3.4',      'f4', ('time_counter',))
        oANOM34    = fid.createVariable('ANOM3.4',      'f4', ('time_counter',))
        # time
        otime[:]          = time
	otime.units       = 'seconds since 0001-01-01 00:00:00'
	otime.calendar    = 'gregorian'
	otime.time_origin = '0001-JAN-01 00:00:00'
        # data
        oNINO12[:]  = NINO12   ; oNINO12.missing_value = -9999.
        oANOM12[:]  = ANOM12   ; oANOM12.missing_value = -9999.
        oNINO3[:]   = NINO3    ; oNINO3.missing_value  = -9999.
        oANOM3[:]   = ANOM3    ; oANOM3.missing_value  = -9999.
        oNINO4[:]   = NINO4    ; oNINO4.missing_value  = -9999.
        oANOM4[:]   = ANOM4    ; oANOM4.missing_value  = -9999.
        oNINO34[:]  = NINO34   ; oNINO34.missing_value = -9999.
        oANOM34[:]  = ANOM34   ; oANOM34.missing_value = -9999.

	units = 'degrees celcius'
	oNINO12.units = units ; oNINO3.units = units ; oNINO4.units = units ; oNINO34.units = units
	oANOM12.units = units ; oANOM3.units = units ; oANOM4.units = units ; oANOM34.units = units
        # close
        fid.close()
        return None


################################################################################################
####################################### Main ###################################################
################################################################################################

def main():
	
	# 1. download files from server
	download_from_web()
	
	# 2. read data in txt files
	year_nino, month_nino, NINO12, ANOM12, NINO3, ANOM3, NINO4, ANOM4, NINO34, ANOM34 = get_data_ninoboxes()
	
	# 3. Setup of time axis
	time_nino = yearmonth_to_seconds(year_nino,month_nino)
	
	# 4. Write data to netcdf file
	write_1d_timeserie_nino('./NC/dmondata_ninoboxes_CPC-NOAA.nc',time_nino,\
	NINO12, ANOM12, NINO3, ANOM3, NINO4, ANOM4, NINO34, ANOM34)
	
	# 5. Clean the txt files
	subprocess.call("rm sstoi.indices soi", shell=True)
	return None


if __name__ == '__main__':
	sys.exit(main())
