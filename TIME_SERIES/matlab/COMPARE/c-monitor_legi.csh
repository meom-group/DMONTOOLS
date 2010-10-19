#!/bin/csh -f
#  $Rev$
#  $Date$
#  $Id$
#--------------------------------------------------------------


if ( $#argv == 0 ) then
   echo 'USAGE: c-monitor.csh config-case1 config_case2 MAXYEAR'
   exit 0
endif

set echo
setenv drakkar_config1 $1
setenv drakkar_config2 $2
setenv MAXYEAR $3

setenv HERE `pwd`
setenv DATAOBSDIR  ${HERE}/../DATA_obs
setenv PLOTDIR     ${HERE}/../PLOTS

set hostweb=meolipc.hmg.inpg.fr
set loginweb=drakkar
set pathweb=DRAKKAR
#  data are in fact in $DATADIR/CONFIG/CONFIG-CASE/DATA/
setenv DATADIR     /net/meolipc/var/www/web/$pathweb

set MATLAB_PATH=/usr/local/bin

set dirfigname=${1}_${2}
set figname=${1}_${2}

set tmp=`echo $1 | sed -e 's/-/ /'`
set config=$tmp[1]
set case=$tmp[2]

nohup $MATLAB_PATH/matlab   -nodesktop -nojvm -nosplash -r CMONITOR  

ssh $hostweb -l $loginweb "if [ ! -d $pathweb/COMPARE ]; \
                         then mkdir $pathweb/COMPARE  ; fi "
ssh $hostweb -l $loginweb "if [ ! -d $pathweb/COMPARE/$dirfigname ]; \
                         then mkdir $pathweb/COMPARE/$dirfigname  ; fi "
ssh $hostweb -l $loginweb "if [ ! -d $pathweb/COMPARE/$dirfigname/TIME_SERIES ]; \
                         then mkdir $pathweb/COMPARE/$dirfigname/TIME_SERIES  ; fi "

ssh $hostweb -l $loginweb "bin/updt_compare.ksh $dirfigname $figname"
scp $PLOTDIR/*.${figname}.jpg $loginweb@${hostweb}:$pathweb/COMPARE/${dirfigname}/TIME_SERIES/

