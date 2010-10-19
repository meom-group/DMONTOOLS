### my python functions :

#####################################################################
## Environement variables
#####################################################################

def get_drakkar_env():
	import os as os
	config   = os.environ['MONPY_CONFIG']
	case     = os.environ['MONPY_CASE']
	frstyear = os.environ['MONPY_FRSTYEAR']
	lastyear = os.environ['MONPY_LASTYEAR']
	datadir  = os.environ['MONPY_DATADIR']
	plotdir  = os.environ['MONPY_PLOTDIR']
	return config, case, frstyear, lastyear, datadir, plotdir

#####################################################################
## Data manipulation
#####################################################################

### find index in array
def getMyLatIndex(array, mylat):
	import matplotlib.pylab as plt
	minidx = plt.NaN 
	for i,v in enumerate(array): 
		if v < mylat: 
			minidx = i+1 
	return minidx


def remove_spval(array, spval, newval):
	import Numeric as Numeric
	maskzeros = Numeric.greater(array, spval)
	Numeric.putmask(array, maskzeros, newval)
	return array

def remove_nan(array):
	import Numeric as Numeric
	import math as math
	temp=[]
	for k in range(len(array)):
		if math.isnan(array[k]) == False :
			temp.append(array[k])
	output=Numeric.array(temp)
	return output

def add_spval(array, value):
	import Numeric as Numeric
	import matplotlib.pylab as plt
	maskzeros = Numeric.equal(array, value)
	Numeric.putmask(array, maskzeros, plt.NaN)
	return array

def extract_month(array, arraymonth, month):
	import Numeric as Numeric
	mask = Numeric.equal(arraymonth, month)
	temp = array * mask
	mylist=[]
	for k in range(len(temp)):
		if temp[k] != 0. :
			mylist.append(temp[k])
	output=Numeric.array(mylist)
	return output



#####################################################################
## Read NetCDF file
#####################################################################

### get the time counter of netcdf file of ID fid
### frstyear is needed to start the serie and allows offset (e.g. climato runs)
def get_time_counter(fich, frstyear):
	import NetCDF as ncdf
	fid=ncdf.NetCDFFile(fich,'r')
	time = (fid.variables['time_counter']).getValue()
	time   = (time - time[0])/86400/365 # in year
	for jt in range(len(time)) :
		year = float(frstyear) + time
	return year

def read_timeserie1d(fich, name):
	import NetCDF as NetCDF
	import Numeric as Numeric
	fid=NetCDF.NetCDFFile(fich,'r')
	value=(fid.variables[name]).getValue()
	value=Numeric.reshape(value, [len(value)])
	return value

def read_dimension1d(fich, name):
	import NetCDF as NetCDF
	import Numeric as Numeric
	fid=NetCDF.NetCDFFile(fich,'r')
	value=(fid.variables[name]).getValue()
	value=Numeric.reshape(value, [max(value.shape)])
	return value

def read_timeserie2d(fich, name, sx, sy):
	import NetCDF as NetCDF
	import Numeric as Numeric
	fid=NetCDF.NetCDFFile(fich,'r')
	value=(fid.variables[name]).getValue()
	value=Numeric.reshape(value, [sx,sy])
	return value

#####################################################################
## Plots
#####################################################################

def plot_trpsig(year, sigma, trpsig, ncontour, titleplot, nameplot):
	import Numeric as Numeric
	import pylab as plt
	transport_total = sum(Numeric.transpose(trpsig))
	maskzeros = Numeric.equal(trpsig, 0.)
	Numeric.putmask(trpsig, maskzeros, plt.NaN)
	plt.close()
	plt.subplot(2,1,1)
	plt.contourf(year, sigma,abs(Numeric.transpose(trpsig)),ncontour)
	plt.grid(True)
	plt.axis([min(year), max(year), max(sigma),min(sigma)])
	plt.ylabel('Sigma classes',fontsize='x-large')
	plt.title(titleplot,fontsize='large')
	plt.colorbar(format='%.3f')
	plt.subplot(2,1,2)
	plt.plot(year, transport_total, 'b')
	plt.grid(True)
	plt.axis([min(year), max(year),
	min(transport_total),max(transport_total)])
	plt.ylabel('Transport',fontsize='x-large')
	plt.savefig(nameplot)


