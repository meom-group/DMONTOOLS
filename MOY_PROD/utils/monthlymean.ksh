#!/bin/ksh

CONFIG=$1
CASE=$2
YEAR=$3
MM=$4
TYP=$5

if [ $# != 5 ] ; then 
   echo 'USAGE : monthlymeans.ksh CONFIG CASE YEAR MM TYP '
   exit
fi

CONFCASE=$CONFIG-$CASE

INDIR=$SDIR/$CONFIG/$CONFCASE-S/$YEAR
OUTDIR=$SDIR/$CONFIG/$CONFCASE-MEAN/$YEAR
#CDF=$WORKDIR/CDFTOOLS_forge/bin # if cdftools are not in your path define this

if [ ! -d $INDIR ] ; then echo "directory $INDIR does not exists..." ; exit ; fi

listfiles=$( ls $INDIR | grep m$MM | grep $TYP | grep $CONFCASE )

for file in $listfiles ; do

    mfget $INDIR/$file

done

#$CDF/cdfmoy_weighted $listfiles # if cdftools are not in your path uncomment this
cdfmoy_weighted $listfiles       # if cdftools are not in your path comment this
mv cdfmoy_weighted.nc ${CONFCASE}_y${YEAR}m${MM}_$TYP.nc
    
