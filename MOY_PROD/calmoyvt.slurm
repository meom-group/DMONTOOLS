#!/bin/ksh
#MSUB -r zmvt_<year>
#MSUB -n  12
#MSUB -T 3600
#MSUB -q standard
#MSUB -o zmvt_<year>.o%I
#MSUB -e zmvt_<year>.e%I
#MSUB -A gen0727

NB_NPROC=$BRIDGE_MSUB_NPROC

set -x
CONFIG=<CONFIG>
CASE=<CASE>

CONFCASE=${CONFIG}-${CASE}
CTL_DIR=<WPDIR>

#. $HOME/.profile
cd $CTL_DIR
. ./config_moy.ksh

liste=''
for m in $(seq 1 12 ) ; do
   mm=$(printf "%02d" $m )
   liste="$liste ./zz_calmoyvt_${mm}_<year>.ksh"
done

#
module load netcdf

runcode  $NB_NPROC <MPITOOLS>/mpi_shell $liste


