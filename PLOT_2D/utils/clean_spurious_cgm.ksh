#!/bin/ksh

CONFIG=$1
CASE=$2
HERE=$( pwd )

PLOTDIR=$SDIR/$CONFIG/PLOTS/$CONFIG-$CASE

echo "Files removed " > $HERE/log.clean
echo "------------- " >> $HERE/log.clean

for subdir in $( ls $PLOTDIR ) ; do

    if [ -d $PLOTDIR/$subdir ] ; then
       cd $PLOTDIR/$subdir
       pwd

       for file in $( ls | grep .cgm ) ; do
   
           sz=$( du $file | awk '{ print $1 }' )
           if [ $sz == 4 ] ; then
              rm $file 
              echo $file has been removed | cat >> $HERE/log.clean
           fi
       done

    fi

done
