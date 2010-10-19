#!/bin/csh -f

if ( $#argv == 0 ) then
   echo 'USAGE: monitor.csh config-case '
   exit 0
endif

set echo
setenv drakkar_config $1

set tmp=`echo $1 | sed -e 's/-/ /'`
set config=$tmp[1]
set case=$tmp[2]

#unsetenv DISPLAY
#-nodisplay -nodesktop -nojvm -nosplash 
nohup /usr/local/bin/matlab   -nodesktop -nojvm -nosplash -r MONITOR_Kiel 
#/home/molines/.matlab6/bin/matlab -nodisplay -nodesktop -nojvm -nosplash -r MONITOR

rcp ${drakkar_config}*jpg apache@meolipc.hmg.inpg.fr:web/DRAKKAR/${config}/${drakkar_config}/TIME_SERIES/
