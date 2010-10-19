#!/bin/csh -f
#  $Rev$
#  $Date$
#  $Id$
#--------------------------------------------------------------


if ( $#argv == 0 ) then
   echo 'USAGE: c-monitor3.csh config-case1 config_case2 config_case3'
   exit 0
endif

set echo
setenv drakkar_config1 $1
setenv drakkar_config2 $2
setenv drakkar_config3 $3
setenv MAXYEAR  $4

setenv HERE `pwd`
setenv DATAOBSDIR  ${HERE}/../DATA_obs
setenv PLOTDIR     ${HERE}/../PLOTS

set hostweb=meolipc.hmg.inpg.fr
set loginweb=drakkar
set pathweb=DRAKKAR
#  data are in fact in $DATADIR/CONFIG/CONFIG-CASE/DATA/
setenv DATADIR     /net/meolipc/var/www/web/$pathweb

set MATLAB_PATH=/usr/local/bin

set dirfigname=3-${1}_${2}_${3}
set figname=${1}_${2}_${3}

set tmp=`echo $1 | sed -e 's/-/ /'`
set config=$tmp[1]
set case=$tmp[2]

nohup $MATLAB_PATH/matlab   -nodesktop -nojvm -nosplash -r CMONITOR3 

ssh $hostweb -l $loginweb "if [ ! -d $pathweb/COMPARE ]; \
                         then mkdir $pathweb/COMPARE  ; fi "
ssh $hostweb -l $loginweb "if [ ! -d $pathweb/COMPARE/$dirfigname ]; \
                         then mkdir $pathweb/COMPARE/$dirfigname  ; fi "
ssh $hostweb -l $loginweb "if [ ! -d $pathweb/COMPARE/$dirfigname/TIME_SERIES ]; \
                         then mkdir $pathweb/COMPARE/$dirfigname/TIME_SERIES  ; fi "
ssh $hostweb -l $loginweb "bin/updt_compare.ksh $dirfigname $figname"

scp $PLOTDIR/*.${figname}.jpg $loginweb@${hostweb}:$pathweb/COMPARE/${dirfigname}/TIME_SERIES/

