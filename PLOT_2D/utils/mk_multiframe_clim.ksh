#!/bin/bash

usage() {
    echo $(basename $0 ) -h -i year1-year2 -c CONFCASE1:CONFCASE2:...
    echo '     -h print this help message'
    echo '     -i year1-year2 : work with climatology year1-year2'
    echo '     -c CONFCASE1:CONFCASE2:... list of CONFCASE to be compared, using '':'' as separator'
    echo
    echo '   This script will build a set of images with all configcase on one frame'
    echo ' The resulting image is thought to have a geometry of 2048 x 1024 px'
    echo ' The size of individual config case will be set according to the number of'
    echo ' frames to be compared.'
    echo '   PLOT layout must follow the DRAKKAR monitoring layout, '
    echo ' eg .../DRAKKAR/ORCA025.L75/ORCA025.L75-MJM95/CLIM_2000-2009'
    exit 0
        }

while getopts :hi:c: opt ; do
   case $opt in
     (h) usage ;;
     (i) clim=${OPTARG} ;;
     (c) list=${OPTARG} ;;
     (*) usage ;;
   esac
done

if [ $# = 0 ] ; then usage ; fi

ROOT_DIR=$HOME/DRAKKAR
OUTDIR=$HOME/${list}_${clim}  # absolute path !
mkdir -p $OUTDIR

# get the list of confcase to compare
alist=( $(echo $list | tr ':' ' ') )
nconfig=${#alist[@]}

echo ${alist[@]}

if [[ $nconfig -le 1 ]] ; then
  echo $nconfig
  usage
fi


case $nconfig in
(2)
   geometry='1024x1024'
   tile='2x1' ;;
(3,4)
   geometry='512x512'
   tile='2x2' ;;
(5,6)
   geometry='512x512'
   tile='3x2' ;;
(*)
   echo Too much confcase to compare 
   exit 1 ;;
esac


# build P_DIR array
i=0
for confcase in ${alist[@]} ; do
  conf[$i]=${confcase%-*}
  kase[$i]=${confcase#*-}   # kase because case is reserved word in ksh ;)
 
  P_DIR[$i]=${ROOT_DIR}/${conf[$i]}/$confcase/CLIM_${clim}
  
  i=$(( i+ 1))
done

# take the first ([0]) as master
cd ${P_DIR[0]}
for dir in * ; do
  if [ -d $dir  ] ; then      
     mkdir -p $OUTDIR/$dir
     cd $dir
     # scan gif files in directory
     for zfig  in *.gif ; do
       # build list of frames to gather
       fig[0]=${P_DIR[0]}/$dir/$zfig   # add absolute path
       i=1 ; skip=0
       while (( $i < $nconfig )) ; do
         fig[$i]=$(echo ${fig[0]} | sed -e "s@${conf[0]}@${conf[$i]}@g" -e "s@${kase[0]}@${kase[$i]}@g" )
         if [ ! -f ${fig[$i]} ] ; then skip=1 ; fi
         i=$(( i + 1 ))
       done
       if [ $skip = 0 ] ; then
         montage -tile $tile -geometry $geometry ${fig[@]} $OUTDIR/$dir/${zfig%-*}.gif
       fi

     done
     cd ..
  fi
done
  





