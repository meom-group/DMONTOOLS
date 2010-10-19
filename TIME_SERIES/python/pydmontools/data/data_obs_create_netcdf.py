###############################################################################################
###                                                                                         ###
### Script data_obs_create_netcdf : creates a NetCDF file with data from observations       ###
###                                 used in the DRAKKAR monitoring. Input from text files,  ###
###                                 output in netcdf. You may have to tune this script if   ###
###                                 your text files format differs from those of JMM.       ###
###                                                                                         ###
###                                 Raphael Dussin, july 2009                               ###
###                                                                                         ###
### HOWTO in the bottom of this file                                                        ###
###############################################################################################

import Scientific.IO.NetCDF
#from Numeric import *
from NetCDF import *

###############################################################################################
### PATHS and FILENAMES
###############################################################################################

dataobsdir='./MTL/'
cablefile='cable.mtl'
elninofile='nino_obs.txt'
soifile='SOIindex.txt'
icenorthfile='North.mtl'
icesouthfile='South.mtl'
### ADD YOUR DATA
#your_file=''

###############################################################################################
### INITIALIZE ARRAYS
###############################################################################################

datalist_cable=['YEAR_CABLE', 'CABLE']

datalist_elnino=['YEAR_ELNINO', 'MONTH_ELNINO', 'NINO1+2', 'ANOM1+2', 'NINO3', 'ANOM3', 'NINO4',
'ANOM4', 'NINO3.4', 'ANOM3.4']

datalist_soi=['YEAR_SOI','MONTH_SOI','SOI']

datalist_icen=['YEAR_ICE_NORTH','MONTH_ICE_NORTH','NORTH_ICE_EXTENT','NORTH_ICE_AREA']

datalist_ices=['YEAR_ICE_SOUTH','MONTH_ICE_SOUTH','SOUTH_ICE_EXTENT','SOUTH_ICE_AREA']

### ADD YOUR DATA
#your_datalist=[]

for k in range(len(datalist_cable)) :
	vars()[datalist_cable[k]]=[]

for k in range(len(datalist_elnino)) :
	vars()[datalist_elnino[k]]=[]

for k in range(len(datalist_soi)) :
	vars()[datalist_soi[k]]=[]

for k in range(len(datalist_icen)) :
	vars()[datalist_icen[k]]=[]

for k in range(len(datalist_ices)) :
	vars()[datalist_ices[k]]=[]

### ADD YOUR DATA
#for k in range(len(your_datalist)) :
#	vars()[your_datalist[k]]=[]

###############################################################################################
### DATA from CABLE (florida-bahamas)
###############################################################################################

f1=open(dataobsdir + cablefile,'r')
lignes1=[lignes1 for lignes1 in f1.readlines() if lignes1.strip() ] # remove empty lines
f1.close()

for chaine in lignes1[:] :
	element=chaine.split()
	for k in range(len(datalist_cable)) :
		vars()[datalist_cable[k]].append(float(element[k]))

###############################################################################################
### DATA from El nino
###############################################################################################

f2=open(dataobsdir + elninofile,'r')
lignes2=[lignes2 for lignes2 in f2.readlines() if lignes2.strip() ] # remove empty lines
f2.close()

for chaine in lignes2 :
	element=chaine.split()
	for k in range(len(datalist_elnino)) :
		vars()[datalist_elnino[k]].append(float(element[k]))

###############################################################################################
### DATA from Southern Oscillation Index (SOI)
###############################################################################################

f3=open(dataobsdir + soifile,'r')
lignes3=[lignes3 for lignes3 in f3.readlines() if lignes3.strip() ] # remove empty lines
f3.close()

nmonth=12

for chaine in lignes3 :
	element=chaine.split()
	for k in range(1,nmonth+1) :
		vars()[datalist_soi[0]].append(float(element[0]))
		vars()[datalist_soi[1]].append(float(k))
	for k in range(1,len(element)) :
		vars()[datalist_soi[2]].append(float(element[k]))

###############################################################################################
### DATA from NOAA Ice in Northern Hemisphere
###############################################################################################

f4=open(dataobsdir + icenorthfile,'r')
lignes4=[lignes4 for lignes4 in f4.readlines() if lignes4.strip() ] # remove empty lines
f4.close()

for chaine in lignes4[0:] :
	element=chaine.split()
	for k in range(len(datalist_icen)) :
		vars()[datalist_icen[k]].append(float(element[k]))

# Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after 
#( Change of satellite SSM/R SSM/I )

indN=vars()[datalist_icen[0]].index(1987)+5

for k in range(indN) :
	vars()[datalist_icen[3]][k]=vars()[datalist_icen[3]][k]+1.19

for k in range(indN,len(vars()[datalist_icen[0]])) :
	vars()[datalist_icen[3]][k]=vars()[datalist_icen[3]][k]+0.31

###############################################################################################
### DATA from NOAA Ice in Southern Hemisphere
###############################################################################################

f5=open(dataobsdir + icesouthfile,'r')
lignes5=[lignes5 for lignes5 in f5.readlines() if lignes5.strip() ] # remove empty lines
f5.close()

for chaine in lignes5[0:] :
	element=chaine.split()
	for k in range(len(datalist_ices)) :
		vars()[datalist_ices[k]].append(float(element[k]))

### ADD YOUR DATA
###############################################################################################
### YOUR DATA 
###############################################################################################

#f6=open(dataobsdir + your_file,'r')
#lignes6=[lignes6 for lignes6 in f6.readlines() if lignes6.strip() ] # remove empty lines
#f6.close()

# HERE IS THE DIFFICULT PART
# YOU HAVE TO FILL YOUR LISTS WITH VALUES FROM THE FILES
# SEE WHAT IS DONE IN THE OTHER CASES AND ADAPT IT TO YOUR CASE

###############################################################################################
### WRITE the NetCDF file
###############################################################################################

fileout=NetCDFFile('./NC/data_obs_DRAKKAR.nc', 'w')

fileout.createDimension('one', 1)
fileout.createDimension('time_cable', len(YEAR_CABLE))
fileout.createDimension('time_elnino', len(YEAR_ELNINO))
fileout.createDimension('time_soi',len(YEAR_SOI))
fileout.createDimension('time_ice_north',len(YEAR_ICE_NORTH))
fileout.createDimension('time_ice_south',len(YEAR_ICE_SOUTH))
### ADD YOUR DATA
#fileout.createDimension('your_time',len(vars()[your_datalist[0]]))

### Cable
varDims = ('one','time_cable')

for k in range(len(datalist_cable)) :
	temp = fileout.createVariable(datalist_cable[k], 'f', varDims)
	temp = fileout.variables[datalist_cable[k]]
	temp.assignValue(vars()[datalist_cable[k]])

### El nino
varDims = ('one','time_elnino')

for k in range(len(datalist_elnino)) :
	temp = fileout.createVariable(datalist_elnino[k], 'f', varDims)
	temp = fileout.variables[datalist_elnino[k]]
	temp.assignValue(vars()[datalist_elnino[k]])

### SOI
varDims = ('one','time_soi')

for k in range(len(datalist_soi)) :
	temp = fileout.createVariable(datalist_soi[k], 'f', varDims)
	temp = fileout.variables[datalist_soi[k]]
	temp.assignValue(vars()[datalist_soi[k]])

### NOAA ICE NORTH
varDims = ('one','time_ice_north')

for k in range(len(datalist_icen)) :
	temp = fileout.createVariable(datalist_icen[k], 'f', varDims)
	temp = fileout.variables[datalist_icen[k]]
	temp.assignValue(vars()[datalist_icen[k]])

### NOAA ICE SOUTH
varDims = ('one','time_ice_south')

for k in range(len(datalist_ices)) :
	temp = fileout.createVariable(datalist_ices[k], 'f', varDims)
	temp = fileout.variables[datalist_ices[k]]
	temp.assignValue(vars()[datalist_ices[k]])

### ADD YOUR DATA
#varDims = ('one','your_time')

#for k in range(len(your_datalist)) :
#	temp = fileout.createVariable(your_datalist[k], 'f', varDims)
#	temp = fileout.variables[your_datalist[k]]
#	temp.assignValue(vars()[your_datalist[k]])

### Close the netCDF file
fileout.close()


###############################################################################################
###                                                                                         ###
###                                  HOW-TO                                                 ###
###                                                                                         ###
###   1) Q : Create my netcdf file, I already have the text files ?                         ###
###                                                                                         ###
###      A : Check your path and filenames then run python this_script.py                   ###
###                                                                                         ###
###   2) Q : Add new data in the netcdf file ?                                              ###
###                                                                                         ###
###      A : In this script, there are commented lines under 'ADD YOUR DATA', uncomment     ###
###          and customize, some steps are straightforward, other more difficult            ###
###                                                                                         ###
###   3) Q : Update the data in the netcdf with updated text files ?                        ###
###                                                                                         ###
###      A : just run python this_script.py                                                 ###
###                                                                                         ###
###############################################################################################
