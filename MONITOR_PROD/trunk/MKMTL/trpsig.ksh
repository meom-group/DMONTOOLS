#!/bin/ksh
#--------------------------------------------------------------
#   $Rev$
#   $Date$
#   $Id$
#--------------------------------------------------------------
#set -x
prefix=..
dir=$( basename `pwd` )
if [ $dir = 'TXT' ] ; then prefix=../.. ; tmp=$( dirname `pwd` ) ; dir=$( basename $tmp ) ; fi

CONFCASE=${dir%-DIAGS}
CONFIG=${CONFCASE%-*}
CASE=${CONFCASE#*-}
MONITORDIR=${prefix}/${CONFCASE}-MONITOR

if [  ! -d $MONITORDIR ] ; then mkdir $MONITORDIR ; fi

n=0
# mini and maxi of sigma0
  mini=25.2 ; maxi=28.5

cd TRPSIG

#for file in ${CONFCASE}*_01_Denmark_strait_trpsig.txt ; do
for file in ${CONFCASE}*_11_Drake_trpsig.txt ; do
   tmp=${file#*_y}
   year=${tmp%%_*.txt}
   fil2=$( echo $file | sed -e 's/11_Drake/14_South_Australia/' )
   n=$(( $n +1 ))
   #awk '{ if ( FNR > 10  && FNR < 20 ) { print } }'
   sig=$( cat $file | grep -v -e '^#' | awk '{ if ( $1 >   25.2  && $1 < 28.5 ) {printf "%8.3f" ,  $1 }}' mini=$mini  maxi=$maxi )
   trp01=$( cat $file | grep -v -e '^#' | awk '{ if ( $1 > 25.2  && $1 < 28.5 ) {printf "%13.4e" ,  $2 } }' mini=$mini  maxi=$maxi)
   trp02=$( cat $fil2 | grep -v -e '^#' | awk '{ if ( $1 > 25.2  && $1 < 28.5 ) {printf "%13.4e" ,  $2 } }' mini=$mini  maxi=$maxi)

   if [ $n = 1 ] ; then
     echo 000000  $sig > ../${CONFCASE}_TRPSIG.mtl
   fi
   echo $year $trp01 >>  ../${CONFCASE}_TRPSIG.mtl
   echo $year $trp02 >>  ../${CONFCASE}_TRPSIG.mtl

done

cd ../
mv ${CONFCASE}_TRPSIG.mtl $MONITORDIR


