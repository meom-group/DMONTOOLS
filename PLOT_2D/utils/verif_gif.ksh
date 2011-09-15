#!/bin/ksh
# This script is used on meolipc or on jade to check 
# if the movies (from PLOT_2D) are complete
#------------------------------------------------------------

usage() {
     echo USAGE: $(basename $0 ) [-h] [-c confcase]
     echo "      -h : help "
     echo "      -c confcase : indicate the config and case to check "
     echo '      This script can be used as following: verif_gif.ksh -c confcase | grep -v " $nb images"'
     exit 0
        }

CONFCASE=''

while getopts :hc: opt ; do
   case $opt in
     (h) usage ;;
     (c) CONFCASE=${OPTARG} ;;
     (*) usage ;;
   esac
done


if [ ! $CONFCASE ] ; then
  echo 'you must specify confcase with -c'
  usage
fi

echo $CONFCASE

CONFIG=$( echo $CONFCASE | awk -F- '{print $1}' )

tmp=$(pwd)

HOSTNAME=$(hostname | sed -e "s/1//" | sed -e "s/2//" | sed -e "s/3//" | sed -e "s/4//")
echo $HOSTNAME | grep -q service
a=$?

if [ $a = 0 ] ; then
  cd $WORKDIR/$CONFIG/PLOTS/$CONFCASE
elif [ $HOSTNAME = meolipc ] ; then
  cd /home/users/drakkar/DRAKKAR/$CONFIG/$CONFCASE
fi

for dir in ATLN ATLS CAMPBELL CIRCUM DRAKE DWBC GLOBAL ICE KERGUELEN MXL OVT SECTIONS SECTIONS1 ; do
   echo $dir
   cd $dir
   for file in *gif ; do 
      gifsicle -I $file | grep images
   done
   cd ..
done

cd $tmp
