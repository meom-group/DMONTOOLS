#!/bin/bash

year=<YYYY>

. ./config_moy
. ./function_moy

set -x

cp ./config_moy    $TMPDIR
cp ./function_moy  $TMPDIR
cp $MPITOOLS/mpi_shell $TMPDIR

cd $TMPDIR
chkdir $VTTMPDIR 
chkdir $VTTMPDIR/$year

liste=''

### Create the jobs for mpi_shell and do the list
for m in $(seq 1 $STEP 12 ) ; do

   mm=$(printf "%02d" $m )
   cat $MPDIR/calmoyvt_mm | sed -e "s/<year>/$year/g" -e "s/<mm>/$mm/g" \
                                    -e "s/<m>/$m/g"                         \
                                    -e "s/##LL##//g" > zz_calmoyvt_${mm}_$year
   chmod 755 zz_calmoyvt_${mm}_$year
   liste="$liste ./zz_calmoyvt_${mm}_$year"

done

### execute mpi_shell
sleep 3
./mpi_shell $liste

### one processor does the annual mean
annualvt
