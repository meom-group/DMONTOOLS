################################################################################################
### Create BERING netcdf file from Rebecca Woodgate
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
	serv    = 'ftp://psc.apl.washington.edu/'
	dirdata = 'BeringStraitArchive/BeringStraitMooringData/'
	fclim   = 'WoodgateetalGRL05_BeringStraitTSVclimatology.txt'
	fuv_1m  = 'WoodgateetalGRL05_BeringStraitUVmonthlymeans90to04all_VersionMay05.txt'
	fts_1m  = 'WoodgateetalGRL05_BeringStraitTSmonthlymeans90to04all_VersionMay05.txt'
	urllib.urlretrieve(serv + dirdata + fclim,filename=fclim)
	urllib.urlretrieve(serv + dirdata + fuv_1m, filename=fuv_1m)
	urllib.urlretrieve(serv + dirdata + fts_1m, filename=fts_1m)
	return fclim, fuv_1m, fts_1m

################################################################################################
####################################### Sort the data ##########################################
################################################################################################

def get_data_clim(filename):
	# read the file
	f = open(filename,'r')
	alllines = [alllines for alllines in f.readlines() if (alllines.find('%') == -1) ] # remove comments
	alllines = alllines[0:14]                                                          # take only 14 first remaining lines
	alllines.remove('\n')                                                              # remove empty lines
	f.close()
	# init lists
	month     = []
	tclim_1m  = [] ; tclim_err_1m = []
	sclim_1m  = [] ; sclim_err_1m = []
	vclim_1m  = [] ; vclim_err_1m = []
	Tclim_1m  = [] 
	#
	# get monthly estimates
	for line in alllines[0:12]:
		elts = line.replace('(',' ').replace(')',' ').split()
		month.append(              elts[0]  )
		tclim_1m.append(     float(elts[1]) )
		tclim_err_1m.append( float(elts[2]) )
		sclim_1m.append(     float(elts[3]) )
		sclim_err_1m.append( float(elts[4]) )
		vclim_1m.append(     float(elts[5]) )
		vclim_err_1m.append( float(elts[6]) )
		Tclim_1m.append(     float(elts[7]) )
	
	# init lists
	tclim_1y  = [] ; tclim_err_1y = []
	sclim_1y  = [] ; sclim_err_1y = []
	vclim_1y  = [] ; vclim_err_1y = []
	Tclim_1y  = [] 
	#
	# get annual estimates
	elts = alllines[12].replace('(',' ').replace(')',' ').split()
	tclim_1y.append(     float(elts[1]) )
	tclim_err_1y.append( float(elts[2]) )
	sclim_1y.append(     float(elts[3]) )
	sclim_err_1y.append( float(elts[4]) )
	vclim_1y.append(     float(elts[5]) )
	vclim_err_1y.append( float(elts[6]) )
	Tclim_1y.append(     float(elts[7]) )

	# convert to numpy arrays
	ztclim_1m = npy.array(tclim_1m)         ; ztclim_1y = npy.array(tclim_1y)
	ztclim_err_1m = npy.array(tclim_err_1m) ; ztclim_err_1y = npy.array(tclim_err_1y)
	zsclim_1m = npy.array(sclim_1m)         ; zsclim_1y = npy.array(sclim_1y)
	zsclim_err_1m = npy.array(sclim_err_1m) ; zsclim_err_1y = npy.array(sclim_err_1y)
	zvclim_1m = npy.array(vclim_1m)         ; zvclim_1y = npy.array(vclim_1y)
	zvclim_err_1m = npy.array(vclim_err_1m) ; zvclim_err_1y = npy.array(vclim_err_1y)
	zTclim_1m = npy.array(Tclim_1m)         ; zTclim_1y = npy.array(Tclim_1y)

	outdict = {'temp_1m': ztclim_1m, 'temp_1y': ztclim_1y,'errtemp_1m': ztclim_err_1m, 'errtemp_1y': ztclim_err_1y, \
	'salt_1m': zsclim_1m, 'salt_1y': zsclim_1y,'errsalt_1m': zsclim_err_1m, 'errsalt_1y': zsclim_err_1y, \
	'velo_1m': zvclim_1m, 'velo_1y': zvclim_1y,'errvelo_1m': zvclim_err_1m, 'errvelo_1y': zvclim_err_1y, \
	'tran_1m': zTclim_1m, 'tran_1y': zTclim_1y,'months' : month }

	return outdict

def find_start_of_interannual_mooring_data(filein,mooring):
	# read the file
	f = open(filein,'r') ; lines = f.readlines() ; f.close()
	# init
	start_line = [] ; kl = 0
	# loop on lines
	for line in lines:
		if line.find(mooring) != -1:
			start_line.append(kl + 2) # two first lines are comments
		kl = kl + 1
	return start_line

def find_end_of_interannual_mooring_data(filein,start):
	# read the file
	f = open(filein,'r') ; lines = f.readlines() ; f.close()
	# init
	end_line = 0 ; kl = start
	# loop on lines
	for line in lines[start:]:
		end_line = kl
		if line.find('%') != -1:
			end_line = end_line - 1
			break
		kl = kl + 1
	return end_line

def read_interannual_data(filein,start):
	final_line = find_end_of_interannual_mooring_data(filein,start) + 1
	# read the file
	f = open(filein,'r') ; lines = f.readlines() ; f.close()
	correct_lines = lines[start:final_line]
	# init
	year = [] ; month = [] ; data_1m = [] ; err_data_1m = []
	# loop on lines
	for line in correct_lines:
		elts = line.split()
		year.append(          int(elts[2]) )
		month.append(         int(elts[3]) )
		data_1m.append(     float(elts[4]) )
		err_data_1m.append( float(elts[5]) )

	# convert to numpy arrays
	zyear = npy.array(year) ; zmonth = npy.array(month)
	zdata_1m = npy.array(data_1m) ; zerr_data_1m = npy.array(err_data_1m)

	# we fill the blanks by special values
	zyear_cor, zmonth_cor, zdata_1m_cor, zerr_data_1m_cor = fill_missing_months(zyear,zmonth,zdata_1m,zerr_data_1m)

	return zyear_cor, zmonth_cor, zdata_1m_cor, zerr_data_1m_cor

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

def monthname_to_number(indict):
	cmonth = indict['months']
	for kt in range(len(cmonth)):
		km = indict['months'][kt]
		if km == 'Jan':
			indict['months'][kt] = 1
		elif km == 'Feb':
			indict['months'][kt] = 2
		elif km == 'Mar':
			indict['months'][kt] = 3
		elif km == 'Apr':
			indict['months'][kt] = 4
		elif km == 'May':
			indict['months'][kt] = 5
		elif km == 'Jun':
			indict['months'][kt] = 6
		elif km == 'July':
			indict['months'][kt] = 7
		elif km == 'Aug':
			indict['months'][kt] = 8
		elif km == 'Sep':
			indict['months'][kt] = 9
		elif km == 'Oct':
			indict['months'][kt] = 10
		elif km == 'Nov':
			indict['months'][kt] = 11
		elif km == 'Dec':
			indict['months'][kt] = 12
		else:
			pass
	outdict = indict
	return outdict

def fill_missing_months(year,month,data,errdata):
	# setup of a new full time axis
	yeartmp = npy.arange(year.min(),year.max()+1)
	yearout = npy.repeat(yeartmp,12)
	monthtmp = npy.arange(1,12+1)
	monthout = monthtmp
	for ky in range(len(yeartmp)-1):
		monthout = npy.concatenate((monthout,monthtmp))
	#
	st = yearout.shape[0]
	dataout    = -9999. * npy.ones((st))
	errdataout = -9999. * npy.ones((st))
	# loop on years and month
	ct = 0
	for ky in range(yearout.min(),yearout.max()+1):
		for km in range(1,12+1):
			# find index in non-uniform vector
			ind = npy.where( (year == ky) & (month == km) )
			#
			if year[ind].shape[0] == 1:
				dataout[ct]    = data[ind]
				errdataout[ct] = errdata[ind]
			ct = ct + 1
	return yearout, monthout, dataout, errdataout

################################################################################################
####################################### NetCDF output ##########################################
################################################################################################

## Write 1d timeserie
def write_1d_timeserie_clim_monthly(ncfile,indict):
        fid = Dataset(ncfile, 'w', format='NETCDF3_CLASSIC')
        fid.description = 'Bering data from : \n Woodgate R. A., K. Aagaard, T. J. Weingartner (2005), \n ' + \
                          'Monthly temperature, salinity, and transport variability of the Bering Strait through flow, \n ' + \
                          'Geophys. Res. Lett., 32, L04601, doi:10.1029/2004GL021880. \n netcdf file created by DMONTOOLS (MEOM CNRS)'
        # dimensions
        fid.createDimension('time_counter', len(indict['months']))
        # variables
        otime          = fid.createVariable('time_counter',    'f4', ('time_counter',))
	otclim_1m      = fid.createVariable('temp_1m',         'f4', ('time_counter',)) 
	osclim_1m      = fid.createVariable('salt_1m',         'f4', ('time_counter',)) 
	ovclim_1m      = fid.createVariable('velo_1m',         'f4', ('time_counter',)) 
	oTclim_1m      = fid.createVariable('tran_1m',         'f4', ('time_counter',)) 
	otclim_err_1m  = fid.createVariable('errtemp_1m',      'f4', ('time_counter',)) 
	osclim_err_1m  = fid.createVariable('errsalt_1m',      'f4', ('time_counter',)) 
	ovclim_err_1m  = fid.createVariable('errvelo_1m',      'f4', ('time_counter',)) 
        # time
        otime[:]          = npy.array(indict['months'])
	#otime.units       = 'seconds since 0001-01-01 00:00:00'
	#otime.calendar    = 'gregorian'
	#otime.time_origin = '0001-JAN-01 00:00:00'
        ## data
        otclim_1m[:] = indict['temp_1m']          ; otclim_1m.missing_value = -9999.
        osclim_1m[:] = indict['salt_1m']          ; osclim_1m.missing_value = -9999.
        ovclim_1m[:] = indict['velo_1m'] / 100.   ; ovclim_1m.missing_value = -9999.
        oTclim_1m[:] = indict['tran_1m']          ; oTclim_1m.missing_value = -9999.

        otclim_err_1m[:] = indict['errtemp_1m']   ; otclim_err_1m.missing_value = -9999.
        osclim_err_1m[:] = indict['errsalt_1m']   ; osclim_err_1m.missing_value = -9999.
        ovclim_err_1m[:] = indict['errvelo_1m']   ; ovclim_err_1m.missing_value = -9999.

	# units 
	otclim_1m.units = 'Deg C' ; otclim_err_1m.units = 'Deg C'
	osclim_1m.units = 'PSU'   ; osclim_err_1m.units = 'PSU'
	ovclim_1m.units = 'm/s'   ; ovclim_err_1m.units = 'm/s'
	oTclim_1m.units = 'Sv'
        # close
        fid.close()
        return None

def write_1d_timeserie_interannual_monthly(ncfile,indict):
        fid = Dataset(ncfile, 'w', format='NETCDF3_CLASSIC')
        fid.description = 'Bering data from : \n Woodgate R. A., K. Aagaard, T. J. Weingartner (2005), \n ' + \
                          'Monthly temperature, salinity, and transport variability of the Bering Strait through flow, \n ' + \
                          'Geophys. Res. Lett., 32, L04601, doi:10.1029/2004GL021880. \n netcdf file created by DMONTOOLS (MEOM CNRS)'
        # dimensions
        fid.createDimension('time_counter', indict['time'].shape[0])
        # variables
        otime          = fid.createVariable('time_counter',    'f4', ('time_counter',))
	otemp_1m       = fid.createVariable('votemper',        'f4', ('time_counter',)) 
	otemp_1m_err   = fid.createVariable('votemper_error',  'f4', ('time_counter',)) 
	osalt_1m       = fid.createVariable('vosaline',        'f4', ('time_counter',)) 
	osalt_1m_err   = fid.createVariable('vosaline_error',  'f4', ('time_counter',)) 
	ouzon_1m       = fid.createVariable('vozocrtx',        'f4', ('time_counter',)) 
	ouzon_1m_err   = fid.createVariable('vozocrtx_error',  'f4', ('time_counter',)) 
	ovmed_1m       = fid.createVariable('vomecrty',        'f4', ('time_counter',)) 
	ovmed_1m_err   = fid.createVariable('vomecrty_error',  'f4', ('time_counter',)) 
        # time
        otime[:]          = npy.array(indict['time'])
	otime.units       = 'seconds since 0001-01-01 00:00:00'
	otime.calendar    = 'gregorian'
	otime.time_origin = '0001-JAN-01 00:00:00'
        ## data
        otemp_1m[:]     = indict['temp_1m']          ; otemp_1m.missing_value = -9999.
        otemp_1m_err[:] = indict['errtemp_1m']       ; otemp_1m_err.missing_value = -9999.
        osalt_1m[:]     = indict['salt_1m']          ; osalt_1m.missing_value = -9999.
        osalt_1m_err[:] = indict['errsalt_1m']       ; osalt_1m_err.missing_value = -9999.
        ouzon_1m[:]     = indict['uzon_1m']    /100. ; ouzon_1m.missing_value = -9999. / 100. # UGLY
        ouzon_1m_err[:] = indict['erruzon_1m'] /100. ; ouzon_1m_err.missing_value = -9999. / 100.
        ovmed_1m[:]     = indict['vmed_1m']    /100. ; ovmed_1m.missing_value = -9999. / 100.
        ovmed_1m_err[:] = indict['errvmed_1m'] /100. ; ovmed_1m_err.missing_value = -9999. / 100.

	# units 
	otemp_1m.units  = 'Deg C' ; otemp_1m_err.units = 'Deg C'
	osalt_1m.units  = 'PSU'   ; osalt_1m_err.units = 'PSU'
	ouzon_1m.units  = 'm/s'   ; ouzon_1m_err.units = 'm/s'
	ovmed_1m.units  = 'm/s'   ; ovmed_1m_err.units = 'm/s'
        # close
        fid.close()
        return None


################################################################################################
####################################### Main ###################################################
################################################################################################

def main():
	
	# 1. download files from server
	fclim, fuv_1m, fts_1m = download_from_web()
	
	# 2. read data in txt files
	climdict = get_data_clim(fclim)
	
	# 3. Setup of time axis
	climdict = monthname_to_number(climdict)
	
	# 4. Write data to netcdf file
	write_1d_timeserie_clim_monthly('./NC/dmondata_BERING_1m_clim.nc', climdict )

	# 5. Read the interannual data
	ind_start_votemper, ind_start_vosaline = find_start_of_interannual_mooring_data(fts_1m,'MA3')
	ind_start_vomecrty, ind_start_vozocrtx = find_start_of_interannual_mooring_data(fuv_1m,'MA3')
	#
	year, month, votemper_1m, err_votemper_1m = read_interannual_data(fts_1m,ind_start_votemper)
	year, month, vosaline_1m, err_vosaline_1m = read_interannual_data(fts_1m,ind_start_vosaline)
	year, month, vomecrty_1m, err_vomecrty_1m = read_interannual_data(fuv_1m,ind_start_vomecrty)
	year, month, vozocrtx_1m, err_vozocrtx_1m = read_interannual_data(fuv_1m,ind_start_vozocrtx)

	# 6. Setup of time axis
	time_inter = yearmonth_to_seconds(year,month)

	# 7. Pack into one dictionary
	interannualdict = {'time': time_inter, 'temp_1m' : votemper_1m, 'errtemp_1m' : err_votemper_1m, \
	                   'salt_1m' : vosaline_1m, 'errsalt_1m' : err_vosaline_1m, \
	                   'uzon_1m' : vozocrtx_1m, 'erruzon_1m' : err_vozocrtx_1m, \
	                   'vmed_1m' : vomecrty_1m, 'errvmed_1m' : err_vomecrty_1m }
	
	# 8. Write data to netcdf file
	write_1d_timeserie_interannual_monthly('./NC/dmondata_BERING_1m_inter.nc',interannualdict)

	# 9. Clean the txt files
	cmd = 'rm ' + fclim + ' ' + fuv_1m + ' ' + fts_1m
	subprocess.call(cmd, shell=True)
	return None


if __name__ == '__main__':
	sys.exit(main())
