#!/bin/ksh

if [ $# = 0 ] ; then 
   echo USAGE:  mvcalmoy.ksh  year
   exit 1
fi


. ./config_moy.ksh
. ./function_moy.ksh

year=$1
here=$(pwd)

chkdir $MEANDIR
chkdir $MEANDIR/$year

err=0

if [ -d $MOYTMPDIR/$year ] ; then 
   cd $MOYTMPDIR/$year
   find . -name "${CONFIG}-${CASE}_y${year}m??${xiosid}_*.nc" -exec mv {} $MEANDIR/$year \;
else
   echo Mean fields not ready for year $year
   err=$(( err + 1 ))
fi

if [ -d $VTTMPDIR/$year ] ; then 
   cd $VTTMPDIR/$year
   find . -name "${CONFIG}-${CASE}_y${year}m??${xiosid}_VT.nc" -exec mv {} $MEANDIR/$year \;
else
   echo Mean VT fields not ready for year $year
   err=$(( err + 1 ))
fi
if [ $err != 0 ] ; then
   echo $err errors have been found. Script stops
   exit 1
fi

cd $here
./calannual.ksh $year


