#!/bin/ksh
## =====================================================================
##     ***  script  RUN_metamon.ksh  ***
## This is a wrapper for submitting metamon.sub from the CDF directory
## =====================================================================
## History : 1.0  !  2008     J.M. Molines      Original code
## ----------------------------------------------------------------------
##  DMONTOOLS_2.0 , MEOM 2012
##  $Id: RUN_metamon.ksh 636 2015-08-19 08:50:48Z molines $
##  Copyright (c) 2012, J.-M. Molines
##  Software governed by the CeCILL licence (Licence/DMONTOOLSCeCILL.txt)
## ----------------------------------------------------------------------
#
if [ $# != 2 ] ; then
   echo USAGE: RUN_metamon.ksh year-init year-end
   exit 0
fi

  year1=$1
  year2=$2

 # if only one year ...
  year2=${year2:=$year1}

  . ./config_def.ksh
  . ./function_def.ksh

  set -x 

if [ $RNDTMPDIR == 0 ] ; then
  
  # move elementary scripts to the local TMPDIR
  chkdir $R_MONITOR
  XTMPDIR=$R_MONITOR
  cp ./config_def.ksh   $XTMPDIR
  cp ./function_def.ksh $XTMPDIR

  # copy some DMONTOOLS utilities
  cp $PRODTOOLS/create_sections_list.ksh   $XTMPDIR
  cp $PRODTOOLS/drakkar_sections_table.txt $XTMPDIR
  cp $PRODTOOLS/drakkar_trpsig_table.txt   $XTMPDIR
  cp $PRODTOOLS/monitor_prod.ksh           $XTMPDIR

fi

  cp $PRODTOOLS/make_ncdf_timeseries.ksh   .
  cp $PRODTOOLS/drakkar_sections_table.txt .
  cp $PRODTOOLS/drakkar_trpsig_table.txt   .
  cp $TIMESERIES/python/pydmontools/scripts/run_monitor_py.ksh        .

  JOBTYPE=serial # this is the default (only for LoadLeveler)
  TASKTRICK='#'

  NB_NODES=1

  if [ $useMPI == 1 ] ; then
     cp $MPITOOLS/mpi_metamon      $XTMPDIR
     JOBTYPE=parallel # we switch to parallel (only for LoadLeveler)
     TASKTRICK=''

     NB_NPROC=$(( year2 - year1 + 1 ))
     # compute the required number of nodes (assuming MPIPROC cores on 1 node)
     NB_NODES=$(( NB_NPROC / MPIPROC ))
     if (( NB_NPROC % MPIPROC != 0 )) ; then NB_NODES=$(( NB_NODES + 1 )) ; fi

     echo This job is asking for $NB_NODES nodes and $NB_NPROC cores
  fi
  year1=$( printf "%04d" $year1)
  year2=$( printf "%04d" $year2)

  # submit the monitoring mpi
  cat $PRODTOOLS/metamon.skel.sub | sed -e "s/key_$MACHINE/PBS/" \
      -e "s/<year1>/$year1/" -e "s/<year2>/$year2/" \
      -e "s@<R_MONITOR>@$R_MONITOR@" -e "s/<NB_NODES>/$NB_NODES/" -e "s/<MAIL>/$MAIL/" \
      -e "s@<QUEUE>@$QUEUE@" -e "s/<ACCOUNT>/$ACCOUNT/"  \
      -e "s/<JOBTYPE>/$JOBTYPE/" -e "s/<NB_NPROC>/$NB_NPROC/g" -e "s/<MPIPROC>/$MPIPROC/g" \
      -e "s/ifloadlev#/$TASKTRICK/g" -e "s/<RNDTMPDIR>/$RNDTMPDIR/" -e "s/<NODETYPE>/$NODETYPE/g"  \
      -e "s/<WALLTIME>/$WALLTIME/" -e "s/<WALL_CLOCK_LIMIT>/$WALL_CLOCK_LIMIT/" > metamon.sub
  chmod +x metamon.sub
  submit  ./metamon.sub
  \rm  metamon.sub

