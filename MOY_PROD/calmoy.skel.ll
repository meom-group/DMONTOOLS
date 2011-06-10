#!/bin/ksh

year=<YYYY>

. ./config_moy.ksh
. ./function_moy.ksh

set -x

cp ./config_moy.ksh    $TMPDIR
cp ./function_moy.ksh  $TMPDIR
cp $MPITOOLS/mpi_shell $TMPDIR

cd $TMPDIR
chkdir $MOYTMPDIR 
chkdir $MOYTMPDIR/$year

liste=''

### Create the jobs for mpi_shell and do the list
for m in $(seq 1 $STEP 12 ) ; do

   mm=$(printf "%02d" $m )
   cat $MPDIR/calmoy_mm.ksh | sed -e "s/<year>/$year/g" -e "s/<mm>/$mm/g" \
                                  -e "s/<m>/$m/g"                         \
                                  -e "s/##LL##//g" > zz_calmoy_${mm}_$year.ksh
   chmod 755 zz_calmoy_${mm}_$year.ksh
   liste="$liste ./zz_calmoy_${mm}_$year.ksh"

done

### execute mpi_shell
sleep 3
./mpi_shell $liste

### one processor does the annual mean
annual


