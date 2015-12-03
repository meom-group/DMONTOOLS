#!/bin/ksh

## utility to link annual files CONFCASE_ANNUAL_grid?.nc 
## with the new nomenclature : CONFCASE_grid?.nc
## 

## NB: ssh part not working

##########################################################################################

## Check if SDIR is defined

if [ -z $SDIR ] ; then
   echo '*** SDIR must be defined, please check that your .profile ( or .cshrc ...) '
   echo '    has DRAKKAR environement variables correctly defined '
   exit 0
fi

## Check for args

if  (( $# < 2 )) || (( $# > 3 )) ; then
    echo '*** USAGE: links_annual_files.ksh CONFIG CASE (optional : REMOTE_MACHINE)'
    echo '*** EXAMPLE : links_annual_files.ksh NATL025 GRD81 ergon'
    exit 0
fi

##########################################################################################

check_abort() { if [ ! -d $MEANY ] ; then 
                  echo "Your diags directory $MEANY does not exists" ; 
                  exit 1 ; fi ; }

list_years() { LYEAR=`cd $MEANY ; ls -l | grep d* | awk '{ print $NF }' ` ; }
make_links() { if [ -d $MEANY/$year ] ; then 
                  cd $MEANY/$year ;
                  for file in ${CONFCASE}_*_ANNUAL*.nc ; do
                     fileout=$( echo $file | sed -e "s/_ANNUAL//" )
                     ln -s $file $fileout
                  done ; 
               fi ; }

##########################################################################################

CONFIG=$1 ; CASE=$2 ; REMOTE=NO
if [ $# == 3 ] ; then
   REMOTE=YES ; rmachine=$3
fi
CONFCASE=${CONFIG}-${CASE}
MEANY=${SDIR}/${CONFIG}/${CONFCASE}-MEAN

## Create list of years
case $REMOTE in
YES) echo 'Working on remote machine not working yet ' ; exit 1 ;;
NO)  echo 'Working on local machine' ;
     check_abort ;
     list_years ;;
esac

echo $LYEAR

case $REMOTE in
NO)   for year in $LYEAR ; do
        make_links 
     done ;;
esac

