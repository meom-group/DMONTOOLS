#!/bin/bash
#MSUB -r zmoy_<year>
#MSUB -n  12
#MSUB -T 3600
#MSUB -q standard
#MSUB -o zmoy_<year>.o%I
#MSUB -e zmoy_<year>.e%I
#MSUB -A gen0727

NB_NPROC=$BRIDGE_MSUB_NPROC
set -x
CONFIG=<CONFIG>
CASE=<CASE>

CONFCASE=${CONFIG}-${CASE}
CTL_DIR=<WPDIR>

#. $HOME/.profile
cd $CTL_DIR
. ./config_moy

liste=''
for m in $(seq 1 12 ) ; do
   mm=$(printf "%02d" $m )
   liste="$liste ./zz_calmoy_${mm}_<year>"
done

#
module load netcdf

runcode  $NB_NPROC <MPITOOLS>/mpi_shell $liste
