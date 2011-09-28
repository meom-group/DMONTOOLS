#!/bin/ksh

CONFIG=$1
CASE=$2
YEAR=$3

CONFCASE=$CONFIG-$CASE

cd $SDIR/$CONFIG/$CONFCASE-MEAN/$YEAR

echo "                                                                                                            "
echo "############################################################################################################"
echo "   IN $CONFIG/$CONFCASE-MEAN/$YEAR :                                                                        "
echo "                                                                                                            "

##############################################################################
### Check the monthly means

  set -A oknframes 0 6 5 7 6 6 6 6 6 6 6 6 7

  for typ in gridT gridU gridV gridW gridT2 gridU2 gridV2 gridW2 icemod ; do

    for month in $(seq 1 12) ; do
   
        mm=$( printf "%02d" $month )
        nframes=$( ncdump -h ${CONFCASE}_y${YEAR}m${mm}_${typ}.nc | grep iweight | tail -1 | awk '{ print $3 }' )

        if [[ $nframes == ${oknframes[${month}]} ]] ; then
            echo ${CONFCASE}_y${YEAR}m${mm}_${typ}.nc is OK
        else
            echo ${CONFCASE}_y${YEAR}m${mm}_${typ}.nc has only $nframes frames
        fi

    done

    echo '------------------------------------------------------------------'

  done

##############################################################################
### Check the annual means

  for typ in gridT gridU gridV gridW gridT2 gridU2 gridV2 gridW2 icemod ; do

      nframes=$( ncdump -h ${CONFCASE}_y${YEAR}_${typ}.nc | grep iweight | tail -1 | awk '{ print $3 }' )

      if [[ $nframes == 73 ]] ; then
          echo ${CONFCASE}_y${YEAR}_${typ}.nc is OK
      else
          echo ${CONFCASE}_y${YEAR}_${typ}.nc has only $nframes frames
      fi

  done

  echo '------------------------------------------------------------------'

