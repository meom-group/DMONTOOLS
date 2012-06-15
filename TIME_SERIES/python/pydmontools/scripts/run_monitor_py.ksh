#!/bin/ksh

## Important :
## To do this step, you must have completed monitor_prod and make_ncdf_timeseries

# We source the config_def and export them so that python can read them :
. ./config_def.ksh
. ./function_def.ksh

export MONPY_CONFIG=$CONFIG
export MONPY_CASE=$CASE
export MONPY_DATADIR=$MONITOR  
export MONPY_PLOTDIR=$PLOTDIR
export MONPY_DATAOBSDIR=$DATAOBSDIR

# ugly trick for ulam due to change in filesystem management
if [ $MACHINE == 'ulam' ] ; then MONPY_DATADIR=$SDIR/$MONPY_DATADIR ; fi

# Create the output directories tree :

chkdir $PLOTDIR/$CONFIG
chkdir $PLOTDIR/$CONFIG/PLOTS
chkdir $PLOTDIR/$CONFIG/PLOTS/${CONFIG}-${CASE}
chkdir $PLOTDIR/$CONFIG/PLOTS/${CONFIG}-${CASE}/TIME_SERIES

# Run the individual scripts :

if [ $ts_cable == 1 ]       ; then cable.py        ; fi
if [ $ts_gib == 1 ]         ; then gib.py          ; fi
if [ $ts_icemonth == 1 ]    ; then icemonth.py     ; fi
if [ $ts_icenoaa == 1 ]     ; then icenoaa.py      ; fi
if [ $ts_icetrd == 1 ]      ; then icetrd.py ; icetrd_min.py            ; fi
if [ $ts_maxmoc == 1 ]      ; then maxmoc.py       ; fi
if [ $ts_maxmoc40 == 1 ]    ; then maxmoc40.py     ; fi
if [ $ts_mht1 == 1 ]        ; then mht1.py         ; fi
if [ $ts_nino == 1 ]        ; then nino.py         ; fi
if [ $ts_tao == 1 ]         ; then tao_profiles.py ; tao_undercurrent.py ; fi
if [ $ts_transports1 == 1 ] ; then transports1.py  ; fi
if [ $ts_trpsig == 1 ]      ; then trpsig.py       ; fi
if [ $ts_tsmean == 1 ]      ; then tsmean.py       ; fi
if [ $ts_tsmean_lev == 1 ]  ; then tsmean_lev.py   ; fi

### copy to website
cd $PLOTDIR/$CONFIG/PLOTS/${CONFIG}-${CASE}/TIME_SERIES
for file in $( ls | grep .png ) ; do
    copy_to_web $file
done

# The end...
