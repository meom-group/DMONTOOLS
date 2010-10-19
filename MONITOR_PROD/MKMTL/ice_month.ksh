#!/bin/ksh
#--------------------------------------------------------------
#   $Rev$
#   $Date$
#   $Id$
#--------------------------------------------------------------
#  ice.ksh : build a line of matlab file for ice output
#set -x
prefix=..
dir=$( basename `pwd` )
if [ $dir = 'TXT' ] ; then prefix=../.. ; tmp=$( dirname `pwd` ) ; dir=$( basename $tmp ) ; fi

CONFCASE=${dir%-DIAGS}
CONFIG=${CONFCASE%-*}
CASE=${CONFCASE#*-}
MONITORDIR=${prefix}/${CONFCASE}-MONITOR

if [  ! -d $MONITORDIR ] ; then mkdir $MONITORDIR ; fi

\rm -f  ${CONFCASE}_icemonth.mtl
# if no icemonth.txt files in the dir, skip
 ls *icemonth.txt 1> /dev/null 2>&1
 if [ $? != 0 ] ; then echo no icemonth to deal with ... ; exit ; fi

n=0


for file in *icemonth.txt
do
n=$(( $n +1 ))
year=$( head -1 $file | awk '{ print $2}' ) 
nvol=$(  cat $file | grep -e 'NVolume' | grep -v NVolumet | awk '{ printf "%.0f  ", $4}' ) 
svol=$(  cat $file | grep -e 'SVolume' | grep -v SVolumet | awk '{ printf "%.0f  ", $4}' )
narea=$(  cat $file | grep -e 'NArea' | awk '{ printf "%.0f  ", $4}' ) 
sarea=$(  cat $file | grep -e 'SArea' | awk '{ printf "%.0f  ", $4}' )
#nextent=$(  cat $file | grep -e 'NExtend' | awk '{ printf "%.0f  ", $4}' )
#sextent=$(  cat $file | grep -e 'SExtend' | awk '{ printf "%.0f  ", $4}' )
nexnsidc=$(  cat $file | grep -e 'NExnsidc' | awk '{ printf "%.0f  ", $4}' )
sexnsidc=$(  cat $file | grep -e 'SExnsidc' | awk '{ printf "%.0f  ", $4}' )

if [ $n == 1 ] ; then
echo "%  Ice diags for " $CONFCASE  > ${CONFCASE}_icemonth.mtl
printf "%s" "% yr  < --------------- ICE VOLUME ARCTIC    ------------------------------------------> " >> ${CONFCASE}_icemonth.mtl
printf "%s" "<------------------- ICE VOLUME ANTARCTIC  ---------------------------------------> " >> ${CONFCASE}_icemonth.mtl
printf "%s" "<------------------- ICE AREA ARCTIC ---------------------------------------------> " >> ${CONFCASE}_icemonth.mtl
printf "%s" "<------------------- ICE AREA ANTARCTIC ------------------------------------------> " >> ${CONFCASE}_icemonth.mtl
#printf "%s" "<------------------- ICE EXTENT ARCTIC -------------------------------------------> " >> ${CONFCASE}_icemonth.mtl
#printf "%s" "<------------------- ICE EXTENT ANTARCTIC ----------------------------------------> " >> ${CONFCASE}_icemonth.mtl
printf "%s" "<------------------- ICE EXTENT NSIDC ARCTIC -------------------------------------> " >> ${CONFCASE}_icemonth.mtl
printf "%s\n" "<------------------- ICE EXTENT NSIDC ANTARCTIC ----------------------------------> " >> ${CONFCASE}_icemonth.mtl
printf "%04d " 0000 >> ${CONFCASE}_icemonth.mtl
for h in N S ; do
    for v in V A E2 ; do
      m=1
      while (( $m <= 12 )) ; do
        printf "    %02d " $m >> ${CONFCASE}_icemonth.mtl
        m=$(( m + 1 ))
      done
    done
done
printf "\n" >> ${CONFCASE}_icemonth.mtl
fi
printf "%04d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d " $year $nvol >> ${CONFCASE}_icemonth.mtl
printf "% 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d " $svol  >> ${CONFCASE}_icemonth.mtl
printf "% 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d " $narea  >> ${CONFCASE}_icemonth.mtl
printf "% 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d " $sarea  >> ${CONFCASE}_icemonth.mtl
#printf "% 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d " $nextent  >> ${CONFCASE}_icemonth.mtl
#printf "% 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d " $sextent  >> ${CONFCASE}_icemonth.mtl
printf "% 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d " $nexnsidc  >> ${CONFCASE}_icemonth.mtl
printf "% 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d % 6d \n" $sexnsidc  >> ${CONFCASE}_icemonth.mtl

done

mv ${CONFCASE}_icemonth.mtl $MONITORDIR


