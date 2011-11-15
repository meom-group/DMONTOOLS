#!/bin/ksh

CONFIG=$1
CASE=$2
YEAR=$3

CONFCASE=$CONFIG-$CASE

##############################################################################
### Preparative step : get number of snapshot

SNAPDIR=$SDIR/$CONFIG/$CONFCASE-S/$YEAR

# default (5days snapshots)
set -A oknframes 0 6 5 7 6 6 6 6 6 6 6 6 7

for month in $( seq 1 12 ) ; do

    mm=$( printf "%02d" $month )
    oknframes[$month]=$( ls $SNAPDIR | grep m$mm | grep gridT | wc -l )

done

lasttag=$( ls $SNAPDIR | grep gridT | tail -1 | sed -e "s/$CONFCASE//" -e "s/_//g" -e "s/gridT.nc//" )

# get the different file types from lasttag
typlist=''
for file in $( ls $SNAPDIR | grep $lasttag ) ; do

    typlist="$typlist $( echo $file | sed -e "s/$CONFCASE//" -e "s/_//g" -e "s/$lasttag//" -e "s/.nc//" )"

done


##############################################################################
cd $SDIR/$CONFIG/$CONFCASE-MEAN/$YEAR

echo "                                                                                                            "
echo "############################################################################################################"
echo "   IN $CONFIG/$CONFCASE-MEAN/$YEAR :                                                                        "
echo "                                                                                                            "

##############################################################################
### Check the monthly means

  #for typ in gridT gridU gridV gridW gridT2 gridU2 gridV2 gridW2 icemod ; do
  for typ in $typlist gridT2 gridU2 gridV2 gridW2 VT ; do

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

  #for typ in gridT gridU gridV gridW gridT2 gridU2 gridV2 gridW2 icemod ; do
  for typ in $typlist gridT2 gridU2 gridV2 gridW2 VT ; do

      nframes=$( ncdump -h ${CONFCASE}_y${YEAR}_${typ}.nc | grep iweight | tail -1 | awk '{ print $3 }' )

      if [[ $nframes == 73 ]] ; then
          echo ${CONFCASE}_y${YEAR}_${typ}.nc is OK
      else
          echo ${CONFCASE}_y${YEAR}_${typ}.nc has only $nframes frames
      fi

  done

  echo '------------------------------------------------------------------'

