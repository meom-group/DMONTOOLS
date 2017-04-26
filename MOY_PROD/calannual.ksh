#!/bin/ksh

if [ $# = 0 ] ; then 
   echo USAGE:  calannual.ksh  year
   exit 1
fi

. ./config_moy.ksh
. ./function_moy.ksh

year=$1
NC4=${NC4:=0}
if [ $NC4 = 1 ] ; then NCOPT='-nc4' ; fi

cd $MEANDIR/$year
for typ in $TYP_LIST VT ; do
  $CDFTOOLS/cdfmoy_weighted -l ${CONFIG}-${CASE}_y${year}m??${xiosid}_$typ.nc \
                            -o ${CONFIG}-${CASE}_y${year}${xiosid}_$typ.nc $NCOPT
  chmod +r  ${CONFIG}-${CASE}_y${year}${xiosid}_$typ.nc
  case $typ in 
  gridT | gridU | gridV | gridW ) 
    $CDFTOOLS/cdfmoy_weighted -l ${CONFIG}-${CASE}_y${year}m??${xiosid}_${typ}2.nc \
                              -o ${CONFIG}-${CASE}_y${year}${xiosid}_${typ}2.nc  $NCOPT
    chmod +r ${CONFIG}-${CASE}_y${year}${xiosid}_${typ}2.nc
  esac
done

#save on archiving system
save_nc ${CONFIG}/${CONFIG}-${CASE}-MEAN/$XIOS/$year/
