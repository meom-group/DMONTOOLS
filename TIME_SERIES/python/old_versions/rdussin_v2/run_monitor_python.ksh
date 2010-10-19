#!/bin/ksh

#################################################################################
###                                                                           ###
###                   TIME SERIES FOR WEB SITE IN PYTHON                      ###
###                                                                           ###
#################################################################################

CONFIG=NATL025
CASE=BRD80
FRSTYEAR=2004
LASTYEAR=2006
DATADIR='/home/rdussin/DRAKKAR/DEV_MONITORING/TIMESERIES'
PLOTDIR='/home/rdussin/DRAKKAR/DEV_MONITORING/PLOTS_TS'

export MONPY_CONFIG=$CONFIG
export MONPY_CASE=$CASE
export MONPY_FRSTYEAR=$FRSTYEAR
export MONPY_LASTYEAR=$LASTYEAR
export MONPY_DATADIR=$DATADIR
export MONPY_PLOTDIR=$PLOTDIR

#################################################################################
###  Nothing to edit below this line                                          ###
#################################################################################

HERE=`pwd`
RUNDIR=${HERE}/RUN_${CONFIG}-${CASE}/

### Creating the directories
if [ ! -d ${PLOTDIR}/${CONFIG} ] ; then mkdir ${PLOTDIR}/${CONFIG} ; fi
if [ ! -d ${PLOTDIR}/${CONFIG}/PLOTS ] ; then mkdir ${PLOTDIR}/${CONFIG}/PLOTS ; fi

if [ ! -d ${PLOTDIR}/${CONFIG}/PLOTS/${CONFIG}-${CASE} ] ; then 
   mkdir ${PLOTDIR}/${CONFIG}/PLOTS/${CONFIG}-${CASE} ; 
fi

if [ ! -d ${PLOTDIR}/${CONFIG}/PLOTS/${CONFIG}-${CASE}/TIME_SERIES ] ; 
   then mkdir ${PLOTDIR}/${CONFIG}/PLOTS/${CONFIG}-${CASE}/TIME_SERIES ; 
fi

if [ ! -d ${HERE}/RUN_${CONFIG}-${CASE} ] ; then mkdir ${HERE}/RUN_${CONFIG}-${CASE} ; fi

python ./pyfiles/tsmean.py
python ./pyfiles/gib.py
python ./pyfiles/tsmean_lev.py
python ./pyfiles/maxmoc.py
python ./pyfiles/maxmoc40.py
python ./pyfiles/mht1.py
python ./pyfiles/cable.py
python ./pyfiles/transports1.py
python ./pyfiles/icemonth.py
python ./pyfiles/icenoaa.py
python ./pyfiles/icetrd.py
python ./pyfiles/trpsig.py
python ./pyfiles/nino.py


