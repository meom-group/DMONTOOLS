#!/bin/ksh
if [ $# = 0 ] ; then
  echo USAGE: mkclim_gif.ksh  year1-year2
  echo "      create the gif file for climato year1-year2"
  echo "      assuming the corresponding cgm already exist."
  echo "      Migrate the obtained gif to the website under"
  echo "      a CLIM_year1-year2 sub directory"
  exit
fi

mkgif() {
   ctrans -device sun -res 1024x1024 $1 > ztmp.sun
   convert ztmp.sun $2
   \rm ztmp.sun
        }

chkdir() {
   if [ ! -d $1 ] ; then mkdir $1 ; fi
         }

copy() {
          ssh drakkar@meolipc.hmg.inpg.fr -l drakkar " if [ ! -d DRAKKAR/$CONFIG ] ; then mkdir DRAKKAR/$CONFIG ; fi "
          ssh drakkar@meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE ; fi "
          ssh drakkar@meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE/CLIM_$INTER ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE/CLIM_$INTER ; fi "
          ssh drakkar@meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE/CLIM_$INTER/${dir} ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE/CLIM_$INTER/${dir} ; fi "
          scp $gif drakkar@meolipc.hmg.inpg.fr:DRAKKAR/$CONFIG/${CONFCASE}/CLIM_$INTER/${dir}/$(basename $gif)
       }

INTER=$1
CONFCASE=$(basename $(pwd) )
CONFIG=${CONFCASE%-*}
CASE=${CONFCASE#*-}
echo $CONFIG
echo $CASE

for cgm in $(find . -name "*${INTER}*cgm" ) ; do

  dir=$(dirname $cgm)
  chkdir $dir/GIFS
  fil=$(basename $cgm)
  gif=$dir/GIFS/${fil%.cgm}.gif

  if [ ! -f $gif ] ; then
    mkgif $cgm $gif
  fi
    copy  
done
