################################################################################################
### Create TAO netcdf file from NOAA/PMEL
### R.Dussin (geekez malin) for DMONTOOLS


### Try import the modules
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

## files have to be downloaded manually from website :
## http://www.pmel.noaa.gov/tao/data_deliv/deliv-nojava.html
## you should get this list of files :

## cur0n110w_mon.cdf  cur0n140w_mon.cdf  cur0n156e_mon.cdf  cur0n165e_mon.cdf  cur0n170w_mon.cdf

## corresponding to fixed depth current (monthly average)

################################################################################################
####################################### Sort the data ##########################################
################################################################################################

def readnc_2d(filenc,var):
	f = Dataset(filenc)
	out = f.variables[var][:,:]
	return out

def readnc_01d(filenc,var):
	f = Dataset(filenc)
	out = f.variables[var][:]
	return out

def mask_spval(array):
	spval = array.max()
	masque = npy.equal(array, spval)
        out = npy.ma.array(array, mask=masque)
        return out

def climato_means(listfiles,inputdir='.'):
	outdict = {}
	for filein in listfiles:
		uzonal_varname = filein.replace('cur','uzonal_').replace('_mon.cdf','')
		depth_varname  = filein.replace('cur','depth_').replace('_mon.cdf','')
		lon_varname    = filein.replace('cur','lon_').replace('_mon.cdf','')
		lat_varname    = filein.replace('cur','lat_').replace('_mon.cdf','')

		# read in nc
		uzonal_2d = readnc_2d(inputdir  + '/' + filein,'U_320')
		depth     = readnc_01d(inputdir + '/' + filein,'depth')
		lon       = readnc_01d(inputdir + '/' + filein,'lon')
		lat       = readnc_01d(inputdir + '/' + filein,'lat')
		# mask special value
		uzonal_2d = mask_spval(uzonal_2d)
		# time-averaging
		uzonal_bar = uzonal_2d.mean(axis=0).squeeze()
		# convert to m/s
		uzonal_bar = uzonal_bar / 100.

		# fill dictionary
		outdict[uzonal_varname] = uzonal_bar
		outdict[depth_varname]  = depth
		outdict[lon_varname]    = lon
		outdict[lat_varname]    = lat

	return outdict

def reshape_data(inputdict):
	# define the shape of arrays = find nbdepth and count nbp
	# nbdepth = max number of levels 
	# nbp = number of profiles
	nbdepth = 0 ; nbp = 0
	for kk in inputdict.keys():
		if kk.find('uzonal') != -1:
			nbdepth = max(nbdepth, len(inputdict[kk]))
			nbp = nbp + 1

	print 'Creating ', nbdepth, ' x ', nbp, ' arrays'

	# init arrays
	uzonal_4d  = npy.zeros((1,nbdepth,1,nbp))
	depth_4d   = npy.zeros((1,nbdepth,1,nbp))
	lon_4d     = npy.zeros((1,nbdepth,1,nbp))
	lat_4d     = npy.zeros((1,nbdepth,1,nbp))

	# sort by increasing longitude
	ct = 0
	# create longitude dict for sorting
	londict = dict((k, inputdict[k]) for k in inputdict.keys() if ( k.find('lon_') != -1 ) )
	# loop on sorted longitudes
	for key, value in sorted(londict.iteritems(), key=lambda (k,v): (v,k)):
		profile      = key.replace('lon_','')
		lon_4d[0,:,0,ct] = inputdict['lon_' + profile]
		lat_4d[0,:,0,ct] = inputdict['lat_' + profile]

		for kdep in range(len(inputdict['depth_' + profile])) :
			depth_4d[0,kdep,0,ct]  = inputdict['depth_'  + profile][kdep]
			uzonal_4d[0,kdep,0,ct] = inputdict['uzonal_' + profile][kdep]

		ct = ct + 1

	return uzonal_4d, depth_4d, lon_4d, lat_4d

################################################################################################
####################################### Time conversion ########################################
################################################################################################

#def yearmonth_to_seconds(yearX,monthX):
#	timeX = npy.zeros(len(yearX))
#	for kt in range(len(yearX)):
#		datestr = str(yearX[kt]) + '-' + str(monthX[kt]).zfill(2) + '-15'
#		days_since_year1 = mdates.datestr2num(datestr)
#		seconds_since_year1 = 86400 * days_since_year1
#		timeX[kt] = seconds_since_year1
#	return timeX

################################################################################################
####################################### NetCDF output ##########################################
################################################################################################

## Write climato for zonal current at different moorings
def write_climato_tao(ncfile,lon,lat,depth,uzonal):
        fid = Dataset(ncfile, 'w', format='NETCDF3_CLASSIC')
        fid.description = 'TAO mean zonal current (NOAA/PMEL) \n file created by DMONTOOLS (MEOM CNRS)'
        # dimensions
        fid.createDimension('time_counter', lon.shape[0])
        fid.createDimension('z',            lon.shape[1])
        fid.createDimension('y',            lon.shape[2])
        fid.createDimension('x',            lon.shape[3])
        # variables
        otime      = fid.createVariable('time_counter', 'f4', ('time_counter',))
        olon       = fid.createVariable('nav_lon',      'f4', ('time_counter','z','y','x',))
        olat       = fid.createVariable('nav_lat',      'f4', ('time_counter','z','y','x',))
        odept      = fid.createVariable('depthu',       'f4', ('time_counter','z','y','x',))
        ouzonal    = fid.createVariable('vozocrtx',     'f4', ('time_counter','z','y','x',))
        # time
        otime[:]          = 0.
	otime.units       = 'seconds since 0001-01-01 00:00:00'
	otime.calendar    = 'gregorian'
	otime.time_origin = '0001-JAN-01 00:00:00'
        # data
        olon[:,:,:,:]    = lon    ; olon.missing_value = 0.
        olat[:,:,:,:]    = lat    ; olat.missing_value = 0.
        odept[:,:,:,:]   = depth  ; odept.missing_value = 0.
        ouzonal[:,:,:,:] = uzonal ; ouzonal.missing_value = 0.

	odept.units   = 'm'
	ouzonal.units = 'm/s'
        # close
        fid.close()
        return None


################################################################################################
####################################### Main ###################################################
################################################################################################

def main():
	
	# 1. compute the mean U profile
	listfiles = [ 'cur0n110w_mon.cdf', 'cur0n140w_mon.cdf', 'cur0n156e_mon.cdf', 'cur0n165e_mon.cdf', 'cur0n170w_mon.cdf' ]
	dataraw = climato_means(listfiles,inputdir='./NC')

	# 2. reshape into 2d (lon/depth) arrays
	uzonal_4d, depth_4d, lon_4d, lat_4d = reshape_data(dataraw)

	print uzonal_4d.shape

	write_climato_tao('./NC/dmondata_tao-equator_clim_PMEL-NOAA.nc',lon_4d,lat_4d,depth_4d,uzonal_4d)
	# 1. download files from server
#	download_from_web()
	
	# 2. read data in txt files
#	year_nino, month_nino, NINO12, ANOM12, NINO3, ANOM3, NINO4, ANOM4, NINO34, ANOM34 = get_data_ninoboxes()
	
	# 3. Setup of time axis
#	time_nino = yearmonth_to_seconds(year_nino,month_nino)
	
	# 4. Write data to netcdf file
#	write_1d_timeserie_nino('./NC/dmondata_ninoboxes_CPC-NOAA.nc',time_nino,\
#	NINO12, ANOM12, NINO3, ANOM3, NINO4, ANOM4, NINO34, ANOM34)
	
	# 5. Clean the txt files
#	subprocess.call("rm sstoi.indices soi", shell=True)
#	return None


if __name__ == '__main__':
	sys.exit(main())
