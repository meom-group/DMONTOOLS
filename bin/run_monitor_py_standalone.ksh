#!/bin/ksh

## Important :
## To do this step, you must have completed monitor_prod and make_ncdf_timeseries

######################################################################################
# copy_to_web : copy the time series figures to the DRAKKAR website
# usage : copy_to_web file
copy_to_web() {
          ssh meolipc.hmg.inpg.fr -l drakkar " if [ ! -d DRAKKAR/$CONFIG ] ; then mkdir DRAKKAR/$CONFIG ; fi "
          ssh meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/${CONFIG}-${CASE} ] ; then mkdir DRAKKAR/$CONFIG/${CONFIG}-${CASE} ; fi "
          ssh meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/${CONFIG}-${CASE}/TIME_SERIES ] ; then mkdir DRAKKAR/$CONFIG/${CONFIG}-${CASE}/TIME_SERIES ; fi "
          scp $1 drakkar@meolipc.hmg.inpg.fr:DRAKKAR/$CONFIG/${CONFIG}-${CASE}/TIME_SERIES/$1 ;}

# CHKDIR  : check the existence of a directory. Create it if not present
chkdir() { if [ ! -d $1 ] ; then mkdir $1 ; fi  ; }

######################################################################################

CONFIG=<CONFIG>
CASE=<CASE>
MONITOR=$SDIR/<CONFIG>/<CONFIG>-<CASE>-MONITOR
PLOTDIR=$SDIR
DATAOBS=$PYDMON_DATADIR

WEBCOPY=true # or false

ts_cable=0
ts_gib=0
ts_icemonth=0
ts_icenoaa=0
ts_icetrd=0
ts_maxmoc=0
ts_maxmoc40=0
ts_mht1=0
ts_nino=0
ts_tao=0
ts_transports1=0
ts_trpsig=0
ts_tsmean=0
ts_tsmean_lev=0

#############################################################################

export MONPY_CONFIG=$CONFIG
export MONPY_CASE=$CASE
export MONPY_DATADIR=$MONITOR  
export MONPY_PLOTDIR=$PLOTDIR
export MONPY_DATAOBSDIR=$DATAOBS

chkdir $PLOTDIR/$CONFIG
chkdir $PLOTDIR/$CONFIG/PLOTS
chkdir $PLOTDIR/$CONFIG/PLOTS/${CONFIG}-${CASE}
chkdir $PLOTDIR/$CONFIG/PLOTS/${CONFIG}-${CASE}/TIME_SERIES

## Monthly diags
if [ $ts_icemonth == 1 ]    ; then icemonth.py     ; fi
if [ $ts_icenoaa == 1 ]     ; then icenoaa.py      ; fi
if [ $ts_nino == 1 ]        ; then nino.py         ; fi
if [ $ts_trpsig == 1 ]      ; then trpsig.py       ; fi

## Annual diags
if [ $ts_icetrd == 1 ]      ; then icetrd.py       ; icetrd_min.py  ; fi
if [ $ts_tao == 1 ]         ; then tao_profiles.py ; tao_undercurrent.py ; fi

## Multi-freq diags
list_freq='1m 1y'

for freq_diags in $list_freq ; do

    if [ $ts_cable == 1 ]       ; then cable.py       -f $freq_diags    ; fi
    if [ $ts_gib == 1 ]         ; then gib.py         -f $freq_diags    ; fi
    if [ $ts_maxmoc == 1 ]      ; then maxmoc.py      -f $freq_diags    ; fi
    if [ $ts_maxmoc40 == 1 ]    ; then maxmoc40.py    -f $freq_diags    ; fi
    if [ $ts_mht1 == 1 ]        ; then mht1.py        -f $freq_diags    ; fi
    if [ $ts_transports1 == 1 ] ; then transports1.py -f $freq_diags    ; fi
    if [ $ts_tsmean == 1 ]      ; then tsmean.py      -f $freq_diags    ; fi
    if [ $ts_tsmean_lev == 1 ]  ; then tsmean_lev.py  -f $freq_diags    ; fi

done

### copy to website
if [ $WEBCOPY == 'true' ] ; then

   cd $PLOTDIR/$CONFIG/PLOTS/${CONFIG}-${CASE}/TIME_SERIES
   for file in $( ls | grep .png ) ; do
       copy_to_web $file
   done

fi
### The end
