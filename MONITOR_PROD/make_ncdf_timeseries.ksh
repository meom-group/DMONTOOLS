#!/bin/ksh

#####################################################################################
### This first part is about dealing with CONFIG and MACHINE dependant things     ###
#####################################################################################
#--------------------------------------------------------------
#   $Rev$
#   $Date$
#   $Id$
#--------------------------------------------------------------

if [ $# == 0 ] ; then
    echo '*** USAGE: make_ncdf_timeseries.ksh CONFIG CASE (DIAGS=DIAGS DIRECTORY) (MONITOR=MONITOR DIRECTORY)'
    exit 0
fi

echo '>>> initialisation...'

if [ $# -le 4 ] ; then
   CONFIG=$1
   CASE=$2
   CONFCASE=${CONFIG}-${CASE}
fi

## MACHINE must be defined in your .profile 
if [ $MACHINE ] ; then
echo '>>> Your machine is defined and is' $MACHINE
else
MACHINE=`hostname`
echo '>>> Your machine is not defined, please do it in your .profile (or .cshrc, .bashrc, ...)'
echo '>>> I am trying to find its name...'
echo '>>> found : '$MACHINE
fi

### Default values
if [ $MACHINE == 'ulam' ] ; then
   DIAGS="$HOMEGAYA/$CONFIG/$CONFCASE-DIAGS/NC"
   MONITOR="$HOMEGAYA/$CONFIG/$CONFCASE-MONITOR"
elif [ $MACHINE == 'jade' ] ; then          # jade
   DIAGS="/scratch/$USER/$CONFIG/$CONFCASE-DIAGS/NC"
   MONITOR="/scratch/$USER/$CONFIG/$CONFCASE-MONITOR"
elif [ $MACHINE == 'meolkerg' ] ; then      # meolkerg
   DIAGS="$ISLANDSPATH_MODEL_SET/$CONFIG/$CONFCASE-DIAGS/NC"
   MONITOR="$ISLANDSPATH_MODEL_SET/$CONFIG/$CONFCASE-MONITOR"
else
if [ $# == 2 ] ; then
echo '*** Your DIAGS and MONITOR directories are not defined by default on this machine'
echo '*** Please give the path to this directory as your fourth and fifth arguments'
exit 1
fi
fi

### Update DIAGS and MONITOR directories if needed
if [ $# == 4 ] ; then
DIAGS=$3
MONITOR=$4
fi

### Security tests
PARENT_DIR=` echo $MONITOR | sed -e "s/$CONFCASE-MONITOR//" `
if [ ! -d $PARENT_DIR ] ; then echo "the directory $PARENT_DIR does not exists... aborting" ; exit 1 ; fi
if [ ! -d $MONITOR ] ; then mkdir $MONITOR ; fi

echo '>>> initialisation successful'

#####################################################################################
### Here I define some shell functions                                            ###
#####################################################################################

## nc_correct_time_counter yearbeg ndays_shift nhours_shift myfile
## only valid if run starts on the 1st of january
nc_correct_time_counter() { yearbeg=$1      ;
                            ndays_shift=$2  ;
                            nhours_shift=$3 ;
                            myfile=$4       ;
                            # Correct units
                            ncatted -a units,time_counter,m,c,"seconds since ${1}-01-${2} ${3}:00:00" ${4} ;
                            # Correct time_origin
                            ncatted -a time_origin,time_counter,m,c,"${1}-JAN-${2} ${3}:00:00" ${4} ; }


# copy_nc_to_web : copy the time series in netcdf to the DRAKKAR website
# usage : copy_nc_to_web file
copy_nc_to_web() {
          ssh meolipc.hmg.inpg.fr -l drakkar " if [ ! -d DRAKKAR/$CONFIG ] ; then mkdir DRAKKAR/$CONFIG ; fi "
          ssh meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE ; fi "
          ssh meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE/DATA ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE/DATA ; fi "
          scp $1 drakkar@meolipc.hmg.inpg.fr:DRAKKAR/$CONFIG/${CONFCASE}/DATA/$1 ;}


#####################################################################################
### This is where the real stuff begins                                           ###
#####################################################################################

cp $DMON_ROOTDIR/MONITOR_PROD/drakkar_sections_table.txt $MONITOR
cp $DMON_ROOTDIR/MONITOR_PROD/drakkar_trpsig_table.txt   $MONITOR

#------------------------------------------------------------------
# Concatenation of DIAGS

LIST_OF_DIAGS_MONTHLY="ICEMONTH MAXMOC MHT NINO TAO TRANSPORTS TRPSIG TSGIB TSMEAN"
LIST_OF_DIAGS_ANNUAL="MAXMOC MHT TAO TRANSPORTS TRPSIG TSGIB TSMEAN" 

cd $DIAGS

## monthly
for diagtyp in $LIST_OF_DIAGS_MONTHLY ; do
    ncrcat -F -O -h ${CONFCASE}_y????_1m_${diagtyp}.nc -o $MONITOR/${CONFCASE}_1m_${diagtyp}.nc
done

## annual
for diagtyp in $LIST_OF_DIAGS_ANNUAL ; do
    ncrcat -F -O -h ${CONFCASE}_y????_1y_${diagtyp}.nc -o $MONITOR/${CONFCASE}_1y_${diagtyp}.nc
done

cp LEVITUS*nc $MONITOR

#------------------------------------------------------------------
# Time counter correction (if needed)

cd $MONITOR

# We take the first file available
listfiles=$( ls ${CONFCASE}*.nc )
ftest=$( echo $listfiles | awk '{ print $1 }' )
# We test if we find the 'Start_date' string in it
stest=$( ncdump -h $ftest | grep Start_date )
# We define a flag
lk_startdate=1
if [ "$stest" == '' ] ; then
   lk_startdate=0
fi

if [ $lk_startdate == 0 ] ; then 

   echo '-------------------------------------------------------'
   echo '>>> The Starting date of the run is not set properly'
   echo '>>> DMONTOOLS will help you fixing this issue'

   echo '>> What is the first year of the run (ex: 1958) ?'

   typeset -Z4 yearbeg
   read yearbeg

   echo '>> What is the output frequency of the model (in days, usually 5) ?'

   read freqindays

   echo ''
   echo 'Please verify...'
   echo 'Run starts the 1st of January' $yearbeg
   echo 'Model outputs are every' $freqindays 'days'
   echo 'are you OK with that ? (y/n)'

   good_ans=0

   while [ $good_ans == 0 ]  ; do

      read answer

      if [ $answer == 'y' ] || [ $answer == 'Y' ] ; then
         echo '>> Correcting time counter !' ; good_ans=1
      elif [ $answer == 'n' ] || [ $answer == 'N' ] ; then
         echo '>> Oh ! Too bad... you have to run this program again' ; good_ans=1
         exit
      else
         echo '>> sorry ?!' ; good_ans=0
      fi

   done

   ## The output should be centered at the middle of the period
   ## so we shift the starting date of 1/2 * freqindays
   total_shift=$(  echo $freqindays  | awk '{ print $1 / 2 }' )
   # ndays_shift is the floor of total_shift
   ndays_shift=$(  echo $total_shift | sed -e "s/\./ /g" | awk '{ print $1 }' )
   # nhours_shift is the difference
   nhours_shift=$(( 24 * ( $total_shift - $ndays_shift ) ))

   ## Setting to correct format
   typeset -Z2 ndays_shift
   typeset -Z2 nhours_shift

   ## Control print
   echo We are shifting the time_counter of $ndays_shift days and $nhours_shift hours towards the beginning

   for mfile in $( ls *.nc | grep ${CONFCASE} ) ; do
       nc_correct_time_counter $yearbeg $ndays_shift $nhours_shift $mfile
   done

fi

### end of concat and merge of netcdf files
#------------------------------------------------------------------
### Utilities : copy to the DRAKKAR website

cd $MONITOR

for file in `ls | grep .nc ` ; do
   copy_nc_to_web $file
done


