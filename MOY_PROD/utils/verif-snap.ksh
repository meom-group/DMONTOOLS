#!/bin/ksh

## A script to verify the presence of outputs in the S directory
## Usage : verif_snap.ksh CONFIG CASE YEAR

CONFIG=$1
CASE=$2
YEAR=$3

CONFCASE=$CONFIG-$CASE

cd $SDIR/$CONFIG/$CONFCASE-S/$YEAR

##############################################################################
### Check the number of tags

listtag=''
ntags=0

for file in $( ls | grep gridT ) ; do
    tag=$( echo $file | sed -e "s/${CONFCASE}_//" -e "s/_gridT.nc//" )
    listtag=" $listtag $tag "
    ntags=$(( $ntags + 1 ))
done

firsttag=$( echo $listtag | awk '{ print $1 }' )
lasttag=$( echo $listtag | awk '{ print $NF }' )

##############################################################################
### Print the results of the verification

echo "                                                                                                            "
echo "############################################################################################################"
echo "   IN $CONFCASE-S/$YEAR :                                                                                   "
echo "                                                                                                            "
echo "   -> $ntags tags are present for gridT, from $firsttag to $lasttag                                         "
echo "                                                                                                            "

##############################################################################
### Check the grids

echo "   -> Check for grids   "
echo ""
for tag in $listtag ; do

    listgrid=''

    for file in $( ls | grep $tag ) ; do
        grid=$( echo $file | sed -e "s/${CONFCASE}_${tag}_//" -e "s/.nc//" )
        listgrid="$listgrid $grid"
    done


echo "      in $tag : $listgrid are present"
done
echo ""

##############################################################################
### Check the size of file

problem='no'

# define reference size of grid files from first tag
jg=0

echo "   -> Check for size   "
echo ""

for grid in $listgrid ; do
#    sizeref[$jg]=$( du ${CONFCASE}_${firsttag}_${grid}.nc | awk '{ print $1 }' )
#    sizeh=$( du -h ${CONFCASE}_${firsttag}_${grid}.nc | awk '{ print $1 }' )
    sizeref[$jg]=$( ls -la ${CONFCASE}_${firsttag}_${grid}.nc | awk '{ print $5 }' )
    sizeh=$( ls -la ${CONFCASE}_${firsttag}_${grid}.nc | awk '{ print $5 }' )
    echo "      file $grid : $sizeh "
    jg=$(( $jg + 1 ))
done 

# loop on other tags
for tag in $listtag ; do

    jg=0
    for grid in $listgrid ; do
       #size[$jg]=$( du ${CONFCASE}_${tag}_${grid}.nc | awk '{ print $1 }' )
       size[$jg]=$( ls -la ${CONFCASE}_${tag}_${grid}.nc | awk '{ print $5 }' )
       if [ ! $size[$jg] == $sizeref[$jg] ] ; then
          echo ""
          echo "     WARNING : file ${CONFCASE}_${tag}_${grid}.nc looks weird"
          problem='yes'
       fi
       jg=$(( $jg + 1 ))
    done

done 

if [ $problem == 'yes' ] ; then
   echo ""
   echo "   You seem to have problems with some files"
else
   echo ""
   echo "   Files look OK (from size criterion at least)"
fi

