#!/bin/ksh
#        make_ncdf_timeseries.ksh
#   This script is used to concatenate yearly monitoring files to multi'
#   years files, ready for time-series plotting. It is mandatory to pass'
#   the CONFIG and CASE to this script either with -a or -C option. '
#   Command line option superseed default options.'

#--------------------------------------------------------------
#   $Rev: 439 $
#   $Date: 2012-04-13 20:45:17 +0200 (Fri, 13 Apr 2012) $
#   $Id: make_ncdf_timeseries.ksh 439 2012-04-13 18:45:17Z dussin $
#--------------------------------------------------------------
# give usage of the script
usage ()          {
          echo '   '
          echo "USAGE: $(basename $0)  [-a | -C CONFIG CASE]  [ -h ] ..."
          echo '        ... [-d diag_dir] [-m monitor_dir] [-y start_year]'
          echo '        ... [-f output_frequency] [-c calendar] [-M machine] [-l]'
          echo '   '
          echo '  PURPOSE: '
          echo '       This script is used to concatenate yearly monitoring files to multi'
          echo '       years files, ready for time-series plotting. It is mandatory to pass'
          echo '       the CONFIG and CASE to this script either with -a or -C option. '
          echo '       Command line option superseed default options.'
          echo '   '
          echo '  MANDATORY ARGUMENTS: one of ... '
          echo '   -a             : take CONFIG and CASE from the actual directory.'
          echo '                   (Requires  to launch the script from CTL/CDF dir.'
          echo '   -C CONFIG CASE : specify CONFIG and CASE name ( 2 separate words).'
          echo '   '
          echo '  OPTIONS: '
          echo '   -h             : print this message.'
          echo '   -d diag_dir    : specify the full path of -DIAGS directory to use.'
          echo '   -m monitor_dir : specify the full path of -MONITOR directory to use.'
          echo '   -y start_year  : specify the starting year of the CONFIG-CASE experiment.'
          echo '   -f output_freq : specify the output frequency in days'
          echo '   -c calendar    : specify the calendar type ( one of : gregorian, noleap, 360d )'
          echo '   -M machine     : indicate the name of the target machine (see option -l) '
          echo '                    for already regognized machines).'
          echo '   -l             : list available pre-defined machines'
          echo '   '
                  }
# retrieve config and case from directory name
look_for_config() {
       dirup=$( dirname $(pwd) )
       if [ $(basename $dirup ) != 'CTL' ] ; then 
         echo ' E R R O R !'
         echo '   -a option can be used only in CTL/CDF directory'
         echo '   Move to there or use -C option.'
         echo ' '
         usage ; exit 1
       else
         CONFCASE=$( basename $( dirname $dirup ) )
         CONFIG=${CONFCASE%-*}
         CASE=${CONFCASE#*-}
       fi
                  }
# check existence of directory
chkdir()          {
       if [ ! -d $1 ] ; then mkdir $1 ; fi
                  }
# copy_nc_to_web : copy the time series in netcdf to the DRAKKAR website
# usage : copy_nc_to_web file
copy_nc_to_web()  {
          ssh meolipc.hmg.inpg.fr -l drakkar " if [ ! -d DRAKKAR/$CONFIG ] ; then mkdir DRAKKAR/$CONFIG ; fi "
          ssh meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE ; fi "
          ssh meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE/DATA ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE/DATA ; fi "
          scp $1 drakkar@meolipc.hmg.inpg.fr:DRAKKAR/$CONFIG/${CONFCASE}/DATA/$1 
                  }

#################################################
###   script starts here                      ###
#################################################
# Start initialization

if [ $# == 0 ] ; then
    usage ; exit 0
    exit 0
fi

 # browse command line
 args=("$@")  # save argument list for further use
 while  getopts :haC:d:m:y:M:lf:c: opt ; do
   case $opt in 
      (h) usage ; exit 0  ;;
      (a) look_for_config ;;
      (C) CONFIG=${OPTARG}  ; i=$(( OPTIND - 1 )) ;
          CASE=${args[$i]}  ; (( OPTIND++ )) 
          CONFCASE=${CONFIG}-${CASE} ;;
      (d) DIAGS=${OPTARG} ;;
      (m) MONITOR=${OPTARG} ;;
      (y) yearbeg=${OPTARG} ;;
      (f) freqinday=${OPTARG}   ;;
      (c) calendar=${OPTARG}   ;;
      (M) MACHINE=${OPTARG} ;;
      (l) echo ' Defaults configuration exists for the following pre-defined machines:' ;
          for m in  ulam jade meolkerg ; do 
            echo '    - ' $m
          done ;
          exit 0 ;;
      (\?) echo "Invalid option: -$OPTARG " ; usage; exit 1 ;;
      (:)  echo "Missing argument for option -$OPTARG " ; usage; exit 1 ;;
   esac
 done

 if [ ! $CONFIG ] ; then
   echo E R R O R : mandatory argument missing ! ; usage ; exit 1
 fi

## MACHINE can  be defined in your .profile 
if [ $MACHINE ] ; then
  echo '>>> Your machine is defined and is' $MACHINE
else
  MACHINE=`hostname`
  echo '>>> Your machine is not defined, please do it in your .profile (or .cshrc, .bashrc, ...)'
  echo '>>>  or use the -M option, if automatic guess fails ...'
  echo '>>> I am trying to guess its name...'
  echo '>>> found : '$MACHINE
fi

ierr=0  # reset error flag
case $MACHINE in 
  ( ulam     ) rootdir=$HOMEGAYA              ;;
  ( jade     ) rootdir=/scratch/$USER         ;;
  ( meolkerg ) rootdir=$ISLANDSPATH_MODEL_SET ;;
  ( *        ) echo " Machine $MACHINE is not pre-defined ";
               if [ $DIAGS ] ; then echo "using DIAGS from command line " ;
                               else (( ierr++ )) ;
                               fi ;
               if [ $MONITOR ] ; then echo "using MONITOR from command line " ;
                               else (( ierr++ )) ;
                               fi ;;
esac

if [ $ierr != 0 ] ; then 
  echo '>>>  E R R O R : DIAGS or MONITOR cannot be inferred.' 
   usage  ; exit 1
fi

# set DIAGS and MONITOR from MACHINE name if necessary
if [ ! $DIAGS   ] ; then DIAGS=$rootdir/$CONFIG/$CONFCASE-DIAGS/NC ; fi
if [ ! $MONITOR ] ; then MONITOR=$rootdir/$CONFIG/$CONFCASE-MONITOR/NC ; fi

# Security tests
PARENT_DIR=$(dirname $MONITOR )
chkdir $PARENT_DIR
chkdir $MONITOR

# End initialization

#####################################################################################
### This is where the real stuff begins                                           ###
#####################################################################################
# Additional function ...

ask_yearbeg()   {
   echo '-------------------------------------------------------'
   echo '>>> The Starting date of the run is not set properly'
   echo '>>> DMONTOOLS will help you fixing this issue'

   echo '>> What is the first year of the run (ex: 1958) ?'
   read yearbeg
   yearbeg=$( printf "%04d" $yearbeg )
   (( iask++ ))
                }
ask_freqinday() {

   echo '>> What is the output frequency of the model (in days, usually 5) ?'

   read freqinday
   (( iask++ ))
                }

ask_calendar()  {

   echo '>> What is the type of calendar for this run ( gregorian (leap), noleap, 360d ?)'

   read calendar
   (( iask++ ))
                }

chkanswer()     {

   echo ''
   echo 'Please verify...'
   echo 'Run starts the 1st of January' $yearbeg
   echo 'Model outputs are every' $freqindays 'days'
   echo "Calendar is  $calendar "
   echo 'are you OK with that ? (y/n)'

   good_ans=0

   while [ $good_ans == 0 ]  ; do

      read answer
      answer=$( echo $answer | tr [a-z] [A-Z] )

      case $answer in 
      ( Y ) echo '>> Correcting time counter !' ; good_ans=1 ;;
      ( N ) echo '>> Oh ! Too bad... you have to run this program again' ; good_ans=1 ; 
            echo '>> You might think of using line options (-y -f -c )' ; usage  ;
            exit 1 ;;
      ( * ) echo '>> sorry ?!' ; good_ans=0 ;;
      esac
   done
                }

cp $DMON_ROOTDIR/MONITOR_PROD/drakkar_sections_table.txt $MONITOR
cp $DMON_ROOTDIR/MONITOR_PROD/drakkar_trpsig_table.txt   $MONITOR

#------------------------------------------------------------------
# Concatenation of DIAGS

cd $DIAGS
# list of all diags available in DIAGS dir : eg : 1y_MAXMOC.nc, 1m_MAXMOC.nc ...
LIST_OF_DIAGS=$( ls -1 ${CONFCASE}*  | awk -F_ '{ printf "%s_%s\n",$3,$4 }' | sort -u )

## 
for diagtyp in $LIST_OF_DIAGS ; do
    ncrcat -F -O -h ${CONFCASE}_y????_${diagtyp} -o $MONITOR/${CONFCASE}_${diagtyp}
done

# Levitus are just ready !
cp LEVITUS*nc $MONITOR

#------------------------------------------------------------------
# Time counter correction (if needed)

cd $MONITOR

for file in ${CONFCASE}*.nc ; do
  ndate0=$(ncdump -h $file | grep -i start_date | awk '{print $3}')
  ndate0=${ndate0:=-1}  # if not set or set to -1, file requires treatment
  if [ $ndate0 == -1 ] ; then
     iask=0
     if [ ! $yearbeg   ] ; then ask_yearbeg   ; fi
     if [ ! $freqinday ] ; then ask_freqinday ; fi
     if [ ! $calendar  ] ; then ask_calendar  ; fi
     if [ $iask != 0   ] ; then chkanswer     ; fi
     yearbeg=$( printf "%04d" $yearbeg )
     secshift=$( echo $freqinday | awk '{ print $1/2.*86400 }' )
     # transform yearbeg/01/01 in timestamp in seconds
     sec0=$( echo $yearbeg | awk '{ cmd=$1" 01 01 00 00 00"; print mktime ( cmd ) }' )
     # shift secshfit backward
     sec=$(( sec0 - secshift ))
     time_units="seconds since $(echo $sec | awk '{ print strftime ("%C%y-%m-%d %H:%M:%S",$1) }' )"
     time_origin=$( echo $sec | awk '{ print strftime ("%C%y-%b-%d %H:%M:%S",$1) }' | tr [a-z] [A-Z] )
     # modify time_counter attribute
     ncatted -a calendar,time_counter,m,c,"$calendar" $file
     ncatted -a units,time_counter,m,c,"$time_units" $file
     ncatted -a time_origin,time_counter,m,c,"$time_origin" $file
  fi
done

### end of concat and merge of netcdf files
#------------------------------------------------------------------
### Utilities : copy to the DRAKKAR website

cd $MONITOR

for file in `ls | grep .nc ` ; do
   copy_nc_to_web $file
done


