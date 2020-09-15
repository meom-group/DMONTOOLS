#!/bin/bash
#MSUB -r zyrly_<year>
#MSUB -n  1
#MSUB -T 3600
#MSUB -q standard
#MSUB -o zyrly_<year>.o%I
#MSUB -e zyrly_<year>.e%I
#MSUB -A gen0727
#MSUB -a <JOBIDMOY>,<JOBIDVT>

#jade -W depend=afterok:<JOBIDMOY>:<JOBIDVT>
#jade -v NB_NODES=1,NB_NPROC=12

set -x
CONFIG=<CONFIG>
CASE=<CASE>

CONFCASE=${CONFIG}-${CASE}
CTL_DIR=<WPDIR>

#. $HOME/.profile
cd $CTL_DIR
. ./config_moy
#
./mvcalmoy <year>


