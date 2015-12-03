#!/bin/ksh
# Load Leveler Script skeleton for monthly and annual mean fields
# valid for ADA, and any LL machine, when the data are kept in the 
# WORKING directory
#  This skeleton is transformed to running script for a given year
# with RUN_calmoy.ksh YEAR, when BATCH is ADA in config_moy.ksh

# @ job_name = zcalmoy

## Output listing location
# @ output = zz-$(job_name)-$(step_name).$(jobid)
# @ error  = $(output)
# @ notification = never

# @ step_name = zmoy<year>
# @ job_type = parallel
# specifique Adapp
# @ requirements = (Feature == "prepost")
# @ total_tasks = 12
# @ wall_clock_limit = 8:00:00
# @ queue

# @ step_name = zmvt<year>
# @ job_type = parallel
# @ total_tasks = 12
# specifique Adapp
# @ requirements = (Feature == "prepost")
# @ wall_clock_limit = 8:00:00
# @ queue

# @ step_name = zyearly<year>
# @ dependency = (zmoy<year> == 0 &&  zmvt<year> == 0 )
# specifique Adapp
# @ requirements = (Feature == "prepost")
# @ wall_clock_limit = 8:00:00
# @ queue

set -x
CONFIG=<CONFIG>
CASE=<CASE>

CONFCASE=${CONFIG}-${CASE}
# define a temporary CTL/CDF dir for holding temporary script (not HOME)
# This WPDIR must be the same as in RUN_calmoy.ADA.ksh !
WPDIR=$WORKDIR/RUN_${CONFIG}/${CONFIG}-${CASE}/CTL/CDF

cd $WPDIR
. ./config_moy.ksh


case $LOADL_STEP_NAME in 

  zmoy<year> )
    liste=''
    for m in $(seq 1 12 ) ; do
      mm=$(printf "%02d" $m )
      liste="$liste ./zz_calmoy_${mm}_<year>.ksh"
    done
    poe $WORKDIR/bin/mpi_shell $liste
  ;;

  zmvt<year> )
    liste=''
    for m in $(seq 1 12 ) ; do
       mm=$(printf "%02d" $m )
       liste="$liste ./zz_calmoyvt_${mm}_<year>.ksh"
    done
    poe $WORKDIR/bin/mpi_shell $liste
  ;;

  zyearly<year> )
   ./mvcalmoy.ksh <year>
  ;;

esac


