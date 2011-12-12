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
    echo '*** USAGE: make_ncdf_timeseries.ksh CONFIG CASE FIRST_YEAR (DIAGS=DIAGS DIRECTORY) (MONITOR=MONITOR DIRECTORY)'
    echo '*** FIRST_YEAR corresponds to the first year of the run, not the monitored timeserie'
    exit 0
fi

echo '>>> initialisation...'

if [ $# -le 5 ] ; then
   CONFIG=$1
   CASE=$2
   CONFCASE=${CONFIG}-${CASE}
   FRSTYEAR=$3
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
   DIAGS="/data/$USER/$CONFIG/$CONFCASE-DIAGS/NC"
   MONITOR="/data/$USER/$CONFIG/$CONFCASE-MONITOR"
else
if [ $# == 3 ] ; then
echo '*** Your DIAGS and MONITOR directories are not defined by default on this machine'
echo '*** Please give the path to this directory as your fourth and fifth arguments'
exit 1
fi
fi

### Update DIAGS and MONITOR directories if needed
if [ $# == 5 ] ; then
DIAGS=$4
MONITOR=$5
fi

### Security tests
PARENT_DIR=` echo $MONITOR | sed -e "s/$CONFCASE-MONITOR//" `
if [ ! -d $PARENT_DIR ] ; then echo "the directory $PARENT_DIR does not exists... aborting" ; exit 1 ; fi
if [ ! -d $MONITOR ] ; then mkdir $MONITOR ; fi

echo '>>> initialisation successful'

#####################################################################################
### Here I define some shell functions                                            ###
#####################################################################################
# nc_rm_missval : remove the missing value attribute for nc variables
#
# usage : nc_rm_missval file
nc_rm_missval() { for f in $* ; do ncatted -O -a missing_value,,d,, $f ; done ; }

# nc_correct_time_counter : correct the time counter to have it in years 
#
# usage : nc_correct_time_counter FIRSTYEAR_of_the_run file
#
nc_correct_time_counter() { down=$(( 365 * 86400 )) ;
                            attr_units='years'
                            attr_origin='A.D.'
                            ncap -F -O -s "time_counter = (time_counter / $down ) + $1" $2 $2 ; 
                            ncatted -O -a units,time_counter,o,c,$attr_units $2 ;
                            ncatted -O -a time_origin,time_counter,o,c,$attr_origin $2 ; }

# ncmerge : make a list of fields in 2 netcdf files ; if fields in file2 are not
#           found in file1, they are added in file1
# usage :   ncmerge file1 file2
#
ncmerge() { listdata1=`ncdump -h $1 | grep float | sed -e "s/(/ /g" | awk '{print $2}' ` ;
            listdata2=`ncdump -h $2 | grep float | sed -e "s/(/ /g" | awk '{print $2}' ` ;
            for var2 in $listdata2 ; do
              add=0 #initialise value to zero
              for var1 in $listdata1 ; do
                if [ $var2 == $var1 ] ; then add=0 ; break ; else ; add=1 ; fi
              done
              if [ $add == 1 ] ; then ncks -F --append -v $var2 $2 -o $1 ; fi
            done }

# rename_trp : rename the variables vtrp, htrp and strp adding a suffix found in
#           drakkar_sections_table.txt and change the long names 
# usage :   rename_trp section file
#
rename_trp() { suffix=` grep $1 drakkar_sections_table.txt | head -1 | awk '{ print $2}' ` ;
               longname=` grep $1 drakkar_sections_table.txt | head -1 | awk '{ print $3}' ` ;
               ncrename -v vtrp,vtrp_$suffix $2 ;
               ncrename -v htrp,htrp_$suffix $2 ;
               ncrename -v strp,strp_$suffix $2 ;
               ncatted  -a long_name,vtrp_$suffix,o,c,${longname}_mass_transport $2 ;
               ncatted  -a long_name,htrp_$suffix,o,c,${longname}_heat_transport $2 ;
               ncatted  -a long_name,strp_$suffix,o,c,${longname}_salt_transport $2 ; }

# rename_trpsig : rename the variables vtrp, htrp and strp adding a suffix found in
#           drakkar_sections_table.txt and change the long names 
# usage :   rename_trp section file
#
rename_trpsig() { suffix=` grep $1 drakkar_trpsig_table.txt | head -1 | awk '{ print $2}' ` ;
                  longname=` grep $1 drakkar_trpsig_table.txt | head -1 | awk '{ print $3}' ` ;
                  ncrename -v sigtrp,sigtrp_$suffix $2 ;
                  ncatted  -a long_name,sigtrp_$suffix,o,c,${longname} $2 ; }

# list_transports : make a list of transports based on drakkar_sections_table.txt
list_transports() { grep '[0-9][0-9]_' drakkar_sections_table.txt | awk '{print $1}' | cat > temp_listsections.txt ;
                    listsections=` sort temp_listsections.txt | uniq -c | awk '{print $2}' ` ;
                    \rm -f temp_listsections.txt ; echo $listsections ; }

# list_trpsig : make a list of trpsig based on drakkar_trpsig_table.txt
list_trpsig() { grep '[0-9][0-9]_' drakkar_trpsig_table.txt | awk '{print $1}' | cat > temp_listtrpsig.txt ;
                    listtrpsig=` sort temp_listtrpsig.txt | uniq -c | awk '{print $2}' ` ;
                    \rm -f temp_listtrpsig.txt ; echo $listtrpsig ; }

# copy_nc_to_web : copy the time series in netcdf to the DRAKKAR website
# usage : copy_nc_to_web file
copy_nc_to_web() {
          ssh meolipc.hmg.inpg.fr -l drakkar " if [ ! -d DRAKKAR/$CONFIG ] ; then mkdir DRAKKAR/$CONFIG ; fi "
          ssh meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE ; fi "
          ssh meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE/DATA ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE/DATA ; fi "
          scp $1 drakkar@meolipc.hmg.inpg.fr:DRAKKAR/$CONFIG/${CONFCASE}/DATA/$1 ;}

# merge_files_into : merge diag files from monitor_prod (for timeseries)
# usage : merge_files_into suffix_of_output_file list of suffix of input files 
# exemple : merge_files_into DATA data1 data2 
#           will merge create and merge timeseries from ${CONFCASE}_y????_data1.nc ${CONFCASE}_y????_data2.nc 
#           and put it into ${CONFCASE}_DATA.nc
merge_files_into() {

        tofile=$1
        shift
        fromfiles=$@

        echo ">>> Working on $tofile diags..."

        domerge=0

        for var in $fromfiles ; do

            if [ -f $DIAGS/${CONFCASE}_y????_${var}.nc ] ; then
               cd $DIAGS ; nc_rm_missval ${CONFCASE}_y????_${var}.nc
               ncrcat -O ${CONFCASE}_y????_${var}.nc -o ${CONFCASE}_${var}.nc
               # this is ugly but bulletproof 
               fileout=${CONFCASE}_$tofile.nc
               \cp ${CONFCASE}_${var}.nc $fileout
               domerge=1
            else
               echo "*** sorry ${CONFCASE}_y????_${var}.nc does not exist ***"
            fi

        done

        if [ $domerge == 1 ] ; then

           for var in $fromfiles ; do
               ncmerge $fileout ${CONFCASE}_${var}.nc
               \rm -f ${CONFCASE}_${var}.nc
           done

           nc_correct_time_counter $FRSTYEAR $fileout
           \mv $fileout $MONITOR/$fileout

        fi ; }

#####################################################################################
### This is where the real stuff begins                                           ###
#####################################################################################

cp drakkar_sections_table.txt $DIAGS
cp drakkar_trpsig_table.txt   $DIAGS

cp drakkar_sections_table.txt $MONITOR
cp drakkar_trpsig_table.txt   $MONITOR

#------------------------------------------------------------------
# MOC

echo '>>> Working on MOC diags...'

domerge=0

for var in Atl_maxmoc40N Atl_maxmoc Atl_maxmoc30S Atl_minmoc Aus_maxmoc Aus_maxmoc50S \
           Aus_minmoc Glo_maxmoc40N Glo_maxmoc Glo_maxmoc30S Glo_maxmoc15S Glo_minmoc \
           Inp_minmoc2 Inp_minmoc30S Inp_minmoc ; do 

    if [ -f $DIAGS/${CONFCASE}_y????_${var}.nc ] ; then
       cd $DIAGS ; nc_rm_missval ${CONFCASE}_y????_${var}.nc
       ncrcat -O ${CONFCASE}_y????_${var}.nc -o ${CONFCASE}_${var}.nc   # concatenation
       nc_correct_time_counter $FRSTYEAR ${CONFCASE}_${var}.nc                      # time counter in years
       ncrename -v maxmoc,maxmoc_${var}           ${CONFCASE}_${var}.nc             # rename fields
       ncrename -v minmoc,minmoc_${var}           ${CONFCASE}_${var}.nc
       ncrename -v latmaxmoc,latmaxmoc_${var}     ${CONFCASE}_${var}.nc
       ncrename -v latminmoc,latminmoc_${var}     ${CONFCASE}_${var}.nc
       ncrename -v depthmaxmoc,depthmaxmoc_${var} ${CONFCASE}_${var}.nc
       ncrename -v depthminmoc,depthminmoc_${var} ${CONFCASE}_${var}.nc
       # this is ugly but bulletproof : define the final file
       fileout=${CONFCASE}_MOC.nc
       \cp ${CONFCASE}_${var}.nc $fileout
       domerge=1      # means there is something to work with
    else
       echo "*** sorry ${CONFCASE}_y????_${var}.nc does not exist ***"
    fi

done

### merging :
if [ $domerge == 1 ] ; then
for var in Atl_maxmoc40N Atl_maxmoc Atl_maxmoc30S Atl_minmoc Aus_maxmoc Aus_maxmoc50S \
           Aus_minmoc Glo_maxmoc40N Glo_maxmoc Glo_maxmoc30S Glo_maxmoc15S Glo_minmoc \
           Inp_minmoc2 Inp_minmoc30S Inp_minmoc ; do

    if [ -f $DIAGS/${CONFCASE}_${var}.nc ] ; then
       ncmerge $fileout ${CONFCASE}_${var}.nc
       \rm -f ${CONFCASE}_${var}.nc
    fi

done
fi

\mv ${CONFCASE}_MOC.nc $MONITOR/${CONFCASE}_MOC.nc

#------------------------------------------------------------------
# EL NINO

echo '>>> Working on EL NINO diags...'

domerge=0

for var in NINO12 NINO34 NINO3 NINO4 ; do

    if [ -f $DIAGS/${CONFCASE}_y????_${var}.nc ] ; then
       cd $DIAGS ; nc_rm_missval ${CONFCASE}_y????_${var}.nc
       ncrcat -O ${CONFCASE}_y????_${var}.nc -o ${CONFCASE}_${var}.nc
       nc_correct_time_counter $FRSTYEAR ${CONFCASE}_${var}.nc
       ncrename -v mean_votemper,votemper_$var ${CONFCASE}_${var}.nc
       domerge=1
    else
       echo "*** sorry ${CONFCASE}_y????_${var}.nc does not exist ***"
    fi

done

if [ $domerge == 1 ] ; then

fileout=${CONFCASE}_NINO.nc
file_nino12=${CONFCASE}_NINO12.nc
file_nino34=${CONFCASE}_NINO34.nc
file_nino3=${CONFCASE}_NINO3.nc
file_nino4=${CONFCASE}_NINO4.nc

\cp $file_nino12 $fileout
ncmerge $fileout $file_nino12
ncmerge $fileout $file_nino34
ncmerge $fileout $file_nino3
ncmerge $fileout $file_nino4
\mv $fileout $MONITOR/$fileout
\rm -f $file_nino12 $file_nino34 $file_nino3 $file_nino4

fi

#------------------------------------------------------------------
# TEMPERATURE AND SALINITY MEANS
#

echo '>>> Working on TSMEAN diags...'

domerge=0

for var in SMEAN SSHMEAN TMEAN ; do

    if [ -f $DIAGS/${CONFCASE}_y????_${var}.nc ] ; then
       cd $DIAGS ; nc_rm_missval ${CONFCASE}_y????_${var}.nc
       ncrcat -O ${CONFCASE}_y????_${var}.nc -o ${CONFCASE}_${var}.nc
       domerge=1
    else
       echo "*** sorry ${CONFCASE}_y????_${var}.nc does not exist ***"
    fi

done

if [ $domerge == 1 ] ; then

filet=${CONFCASE}_TMEAN.nc
files=${CONFCASE}_SMEAN.nc
filessh=${CONFCASE}_SSHMEAN.nc
fileout=${CONFCASE}_TSMEAN.nc

\cp $filet $fileout
ncmerge $fileout $files
ncmerge $fileout $filessh

nc_correct_time_counter $FRSTYEAR $fileout
\mv $fileout $MONITOR/$fileout
\rm -f $filet $files $filessh

filelevt=LEVITUS_y0000_TMEAN.nc
filelevs=LEVITUS_y0000_SMEAN.nc
fileout=LEVITUS_y0000_TSMEAN.nc

\cp $filelevt $fileout
ncmerge $fileout $filelevs

mv $fileout $MONITOR/$fileout

fi

#------------------------------------------------------------------
# GIBRALTAR

echo '>>> Working on GIB diags...'

domerge=0

for var in SGIB TGIB ; do

    if [ -f $DIAGS/${CONFCASE}_y????_${var}.nc ] ; then
       cd $DIAGS ; nc_rm_missval ${CONFCASE}_y????_${var}.nc
       ncrcat -O ${CONFCASE}_y????_${var}.nc -o ${CONFCASE}_${var}.nc
       domerge=1
    else
       echo "*** sorry ${CONFCASE}_y????_${var}.nc does not exist ***"
    fi

done

if [ $domerge == 1 ] ; then

filet=${CONFCASE}_TGIB.nc
files=${CONFCASE}_SGIB.nc
fileout=${CONFCASE}_TSGIB.nc

\cp $filet $fileout
ncmerge $fileout $files

nc_correct_time_counter $FRSTYEAR $fileout
\mv $fileout $MONITOR/$fileout
\rm -f $filet $files 

filelevt=LEVITUS_y0000_TGIB.nc
filelevs=LEVITUS_y0000_SGIB.nc
fileout=LEVITUS_y0000_TSGIB.nc

\cp $filelevt $fileout
ncmerge $fileout $filelevs

\mv $fileout $MONITOR/$fileout

fi

#------------------------------------------------------------------
# HEAT FLUXES AND TRANSPORT

echo '>>> Working on HEAT diags...'

domerge=0

for var in mhst hflx ; do

    if [ -f $DIAGS/${CONFCASE}_y????_${var}.nc ] ; then
       cd $DIAGS ; nc_rm_missval ${CONFCASE}_y????_${var}.nc
       ncrcat -O ${CONFCASE}_y????_${var}.nc -o ${CONFCASE}_${var}.nc
       domerge=1
    else
       echo "*** sorry ${CONFCASE}_y????_${var}.nc does not exist ***"
    fi

done

if [ $domerge == 1 ] ; then

filemhst=${CONFCASE}_mhst.nc
fileflx=${CONFCASE}_hflx.nc
fileout=${CONFCASE}_HEAT.nc

\cp $filemhst $fileout
ncmerge $fileout $fileflx

nc_correct_time_counter $FRSTYEAR $fileout
\mv $fileout $MONITOR/$fileout
\rm -f $filemhst $fileflx 

fi

#------------------------------------------------------------------
# TRANSPORTS ALONG SECTIONS

echo '>>> Working on TRANSPORTS diags...'

domerge=0

for var in `list_transports` ; do

    if [ -f $DIAGS/${CONFCASE}_y????_${var}_transports.nc ] ; then
       cd $DIAGS ; nc_rm_missval ${CONFCASE}_y????_${var}_transports.nc
       ncrcat -O ${CONFCASE}_y????_${var}_transports.nc -o ${CONFCASE}_${var}_transports.nc
       rename_trp $var ${CONFCASE}_${var}_transports.nc
       # this is ugly but bulletproof 
       fileout=${CONFCASE}_TRANSPORTS.nc
       \cp ${CONFCASE}_${var}_transports.nc $fileout
       domerge=1
    else
       echo "*** sorry ${CONFCASE}_y????_${var}.nc does not exist ***"
    fi

done

if [ $domerge == 1 ] ; then

   for var in `list_transports` ; do 
       ncmerge $fileout ${CONFCASE}_${var}_transports.nc 
       \rm -f ${CONFCASE}_${var}_transports.nc 
   done

   nc_correct_time_counter $FRSTYEAR $fileout
   \mv $fileout $MONITOR/$fileout

fi

#------------------------------------------------------------------
# ICE

echo '>>> Working on ICE diags...'

domerge=0

for var in icemonth ; do

    if [ -f $DIAGS/${CONFCASE}_y????_${var}.nc ] ; then
       cd $DIAGS ; nc_rm_missval ${CONFCASE}_y????_${var}.nc
       ncrcat -O ${CONFCASE}_y????_${var}.nc -o ${CONFCASE}_${var}.nc
       domerge=1
    else
       echo "*** sorry ${CONFCASE}_y????_${var}.nc does not exist ***"
    fi

done

if [ $domerge == 1 ] ; then

fileicemonth=${CONFCASE}_icemonth.nc
fileout=${CONFCASE}_ICEMONTH.nc
\cp $fileicemonth $fileout

nc_correct_time_counter $FRSTYEAR $fileout
\mv $fileout $MONITOR/$fileout
\rm -f $fileicemonth  

fi

#------------------------------------------------------------------
# TRANSPORTS IN SIGMA CLASSES

echo '>>> Working on TRPSIG diags...'

domerge=0
mylist_trpsig=''

for var in `list_trpsig` ; do

    if [ -f $DIAGS/${CONFCASE}_y????_${var}_trpsig.nc ] ; then
       cd $DIAGS ; nc_rm_missval ${CONFCASE}_y????_${var}_trpsig.nc
       ncrcat -O ${CONFCASE}_y????_${var}_trpsig.nc -o ${CONFCASE}_${var}_trpsig.nc
       rename_trpsig $var ${CONFCASE}_${var}_trpsig.nc
       # this is ugly but bulletproof 
       fileout=${CONFCASE}_TRPSIG.nc
       \cp ${CONFCASE}_${var}_trpsig.nc $fileout
       domerge=1
       mylist_trpsig="$mylist_trpsig $var"
    else
       echo "*** sorry ${CONFCASE}_y????_${var}.nc does not exist ***"
    fi

done

if [ $domerge == 1 ] ; then

   for var in $mylist_trpsig ; do 
       ncmerge $fileout ${CONFCASE}_${var}_trpsig.nc 
       \rm -f ${CONFCASE}_${var}_trpsig.nc
   done

   nc_correct_time_counter $FRSTYEAR $fileout
   \mv $fileout $MONITOR/$fileout

fi

#------------------------------------------------------------------
# TAO

list_vars="VELOCITY_0n110w    VELOCITY_0n140w    VELOCITY_0n156e    VELOCITY_0n165e    VELOCITY_0n170w  
           VELOCITY_0n110w_UC VELOCITY_0n140w_UC VELOCITY_0n156e_UC VELOCITY_0n165e_UC VELOCITY_0n170w_UC"

merge_files_into TAO $list_vars

### end of concat and merge of netcdf files
#------------------------------------------------------------------
### Utilities : copy to the DRAKKAR website

cd $MONITOR

for file in `ls | grep .nc ` ; do
   copy_nc_to_web $file
done


