#!/bin/ksh
#--------------------------------------------------------------
#   $Rev$
#   $Date$
#   $Id$
#--------------------------------------------------------------
#  section.ksh : build a line of matlab file for section output
prefix=../
dir=$( basename `pwd` )
if [ $dir = 'TXT' ] ; then prefix=../../ ; tmp=$( dirname `pwd` ) ; dir=$( basename $tmp ) ; fi

CONFCASE=${dir%-DIAGS}
CONFIG=${CONFCASE%-*}
CASE=${CONFCASE#*-}
MONITORDIR=${prefix}/${CONFCASE}-MONITOR

if [  ! -d $MONITORDIR ] ; then mkdir $MONITORDIR ; fi

\rm -f ${CONFCASE}_matrix.mtl
# if no section_monitor.txt files in the dir, skip
 ls *section_monitor.txt 1> /dev/null 2>&1
 if [ $? != 0 ] ; then echo no sections to deal with ... ; exit ; fi

n=1
for file in *section_monitor.txt ;  do
  year=$( head -1 $file )
  mass=$( cat $file | grep Mass | awk '{ printf "%8.3f" ,  $4 }' )
  heat=$( cat $file | grep Heat | awk '{ printf "%8.3f" ,  $4 }' )
  salt=$( cat $file | grep Salt | awk '{ printf "%8.1f" ,  $4 *1. }' )
  if (( $n == 1 )) ; then
    ns=$( echo $mass | wc -w )
    secname=$( cat $file | grep -e "^ [0-9][0-9]_" )
    echo "% Transport across section for " $CONFCASE "  MASS    HEAT     SALT   "> ${CONFCASE}_matrix.mtl
    echo "% " $ns "  sections "$secname >>  ${CONFCASE}_matrix.mtl
    printf "%s" "% year " >> ${CONFCASE}_matrix.mtl
    for typ in M H S ; do 
      sect=1
    for i in $mass ; do
      printf "    %02d      " $sect >>  ${CONFCASE}_matrix.mtl
      sect=$(( sect + 1 ))
    done
    done
      printf "%s\n" "  SSHMEAN      TMEAN        SMEAN   " >>  ${CONFCASE}_matrix.mtl
    n=$(( n + 1 ))
  fi

  f=${CONFCASE}_y${year}_SSHMEAN.txt
  sshmean=$( cat $f | grep ocean | awk '{ printf " %8.4f " , $6 }' )

  f=${CONFCASE}_y${year}_TMEAN.txt
  tmean=$( cat $f | grep ocean | awk '{ printf " %8.4f " , $6 }' )

  f=${CONFCASE}_y${year}_SMEAN.txt
  smean=$( cat $f | grep ocean | awk '{ printf " %8.4f " , $6 }' )
# add MOC min max for years ATL INP GLO
# add cdf mean SSH

printf "%4d " $year >>  ${CONFCASE}_matrix.mtl
echo $mass | awk '{ for ( i=1 ; i<= NF ; i++ ) printf "% 11.4f ", $i }' >> ${CONFCASE}_matrix.mtl
echo $heat | awk '{ for ( i=1 ; i<= NF ; i++ ) printf "% 11.4f ", $i }' >> ${CONFCASE}_matrix.mtl
echo $salt | awk '{ for ( i=1 ; i<= NF ; i++ ) printf "% 11.4f ", $i }' >> ${CONFCASE}_matrix.mtl
echo $sshmean $tmean $smean | awk '{ for ( i=1 ; i<= NF ; i++ ) printf "% 11.4f ", $i }' >> ${CONFCASE}_matrix.mtl
printf "\n" >>  ${CONFCASE}_matrix.mtl

done

mv ${CONFCASE}_matrix.mtl $MONITORDIR

