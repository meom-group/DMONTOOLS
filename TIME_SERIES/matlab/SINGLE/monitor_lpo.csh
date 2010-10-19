#!/bin/csh -f
#  $Rev$
#  $Date$
#  $Id$
#--------------------------------------------------------------

if ( $#argv == 0 ) then
   echo 'USAGE: monitor.csh config-case '
   exit 0
endif

set echo
setenv drakkar_config $1
setenv HERE `pwd`
setenv DATAOBSDIR  ${HERE}/../DATA_obs
setenv PLOTDIR     ${HERE}/../PLOTS

### LOCAL CUSTOMIZATION ###
#  data are in fact in $DATADIR/CONFIG/CONFIG-CASE/DATA/
setenv DATADIR     ${HERE}/../DATA
# FOR BREST USERS
set MATLAB_PATH=/home1/triagoz/matlab/matlab_last/bin
###

set tmp=`echo $1 | sed -e 's/-/ /'`
set config=$tmp[1]
set case=$tmp[2]

# monitoring web-site in grenoble
set hostweb=meolipc.hmg.inpg.fr
set loginweb=drakkar
set pathweb=/var/www/web/DRAKKAR

scp $loginweb@${hostweb}:$pathweb/${config}/${drakkar_config}/DATA/* ${DATADIR}/.

nohup ${MATLAB_PATH}/matlab -nodesktop -nojvm -nosplash -r MONITOR  

ssh $hostweb -l $loginweb "if [ ! -d $pathweb/${config}/${drakkar_config}/TIME_SERIES ]; \
                         then mkdir $pathweb/${config}/${drakkar_config}/TIME_SERIES ; fi "

scp $PLOTDIR/${drakkar_config}_*jpg $loginweb@${hostweb}:$pathweb/${config}/${drakkar_config}/TIME_SERIES/
