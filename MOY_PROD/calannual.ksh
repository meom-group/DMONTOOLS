#!/bin/ksh

if [ $# = 0 ] ; then 
   echo USAGE:  calannual.ksh  year
   exit 1
fi


. ./config_moy.ksh
. ./function_moy.ksh

year=$1

cd $MEANDIR/$year
for typ in $TYP_LIST VT ; do
  $CDFTOOLS/cdfmoy_weighted ${CONFIG}-${CASE}_y${year}m??_$typ.nc 
  mv  cdfmoy_weighted.nc ${CONFIG}-${CASE}_y${year}_$typ.nc
  case $typ in 
  gridT | gridU | gridV | gridW ) 
    $CDFTOOLS/cdfmoy_weighted ${CONFIG}-${CASE}_y${year}m??_${typ}2.nc 
    mv  cdfmoy_weighted.nc ${CONFIG}-${CASE}_y${year}_${typ}2.nc  ;;
  esac
done
