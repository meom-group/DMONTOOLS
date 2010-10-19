#!/bin/ksh
#--------------------------------------------------------------
#   $Rev$
#   $Date$
#   $Id$
#--------------------------------------------------------------
# This script scan the years of hflx and heat diags
prefix=..
dir=$( basename `pwd` )
if [ $dir = 'TXT' ] ; then prefix=../.. ; tmp=$( dirname `pwd` ) ; dir=$( basename $tmp ) ; fi

CONFCASE=${dir%-DIAGS}
CONFIG=${CONFCASE%-*}
CASE=${CONFCASE#*-}
MONITORDIR=${prefix}/${CONFCASE}-MONITOR

if [  ! -d $MONITORDIR ] ; then mkdir $MONITORDIR ; fi

 \rm -f heat.mtl
# if no heattrp.dat files in the dir, skip
 ls *heattrp.dat 1> /dev/null 2>&1
 if [ $? != 0 ] ; then echo no heattrp to deal with ... ; exit ; fi
 ls *hflx.dat 1> /dev/null 2>&1
 if [ $? != 0 ] ; then echo no hflx to deal with ... ; exit ; fi

n=0
for f in ${CONFCASE}_y????_heattrp.dat ; do 
 n=$(( n + 1 ))
 year=$( echo $f | sed -e "s/${CONFCASE}_y//" -e 's/_heattrp.dat//' )
 flx=$( echo $f | sed -e 's/heattrp/hflx/' )

if [ $n == 1 ] ; then
# output latitude (North to South) on the first row
 echo  -n 0000 >>  heat.mtl
 cat $f  |\
  awk 'BEGIN{s=0} \
       { if (NR > 2) {printf " %8.3f ", $2} \
       }\
       END{ printf "\n" }'   >> heat.mtl
fi

# Global Ocean
  echo -n $year >> heat.mtl
 cat $f  |\
  awk 'BEGIN{s=0} \
       { if (NR > 2) {printf " %8.3f ", $3} \
       }\
       END{ printf "\n" }'   >> heat.mtl

  echo -n $year >> heat.mtl
 cat $flx  |\
  awk 'BEGIN{s=0} \
       { if (NR > 2) {printf " %8.3f ", $3} \
       }\
       END{ printf "\n" }'   >> heat.mtl


# Atlantic Ocean
  echo -n $year >> heat.mtl
 cat $f  |\
  awk 'BEGIN{s=0} \
       { if (NR > 2) {printf " %8.3f ", $4} \
       }\
       END{ printf "\n" }'   >> heat.mtl

  echo -n $year >> heat.mtl
 cat $flx  |\
  awk 'BEGIN{s=0} \
       { if (NR > 2) {printf " %8.3f ", $4} \
       }\
       END{ printf "\n" }'   >> heat.mtl

# Indo-Pacific Ocean
 echo -n $year >> heat.mtl
 cat $f  |\
  awk 'BEGIN{s=0} \
       { if (NR > 2) {printf " %8.3f ", $5 + $6 } \
       }\
       END{ printf "\n" }'   >> heat.mtl

 echo -n $year >> heat.mtl
 cat $flx  |\
  awk 'BEGIN{s=0} \
       { if (NR > 2) {printf " %8.3f ", $5 } \
       }\
       END{ printf "\n" }'   >> heat.mtl

done

mv heat.mtl $MONITORDIR/${CONFCASE}_heat.mtl
 
