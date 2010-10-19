#!/bin/ksh

# $Id: monitor_python_zephir.skel.ksh 18 2009-03-03 cdufour $ 
# $Date: 2009-03-03 09:52:08 +0100 (Tue, 03 Mar 2009) $
# $Rev: 18 $
#################################################################################
###                                                                           ###
###                   TIME SERIES FOR WEB SITE IN PYTHON                      ###
###                                                                           ###
#################################################################################

CONFIG=PERIANT05
CASE=GCdmpH4

RUNDIR=${HOME}/PLOT_MONITOR/MONITOR_PY/RUN_${CONFIG}-${CASE}

if [ ! -d ${STOCKDIR}/${CONFIG} ] ; then mkdir ${STOCKDIR}/${CONFIG} ; fi
if [ ! -d ${STOCKDIR}/${CONFIG}/PLOTS ] ; then mkdir ${STOCKDIR}/${CONFIG}/PLOTS ; fi
if [ ! -d ${STOCKDIR}/${CONFIG}/PLOTS/${CONFIG}-${CASE} ] ; then mkdir ${STOCKDIR}/${CONFIG}/PLOTS/${CONFIG}-${CASE} ; fi
if [ ! -d ${STOCKDIR}/${CONFIG}/PLOTS/${CONFIG}-${CASE}/TIME_SERIES ] ; then mkdir ${STOCKDIR}/${CONFIG}/PLOTS/${CONFIG}-${CASE}/TIME_SERIES ; fi
if [ ! -d ${HOME}/PLOT_MONITOR/MONITOR_PY/RUN_${CONFIG}-${CASE} ] ; then mkdir ${HOME}/PLOT_MONITOR/MONITOR_PY/RUN_${CONFIG}-${CASE} ; fi

#cat ./SKELS/maxmoc.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/maxmoc.py
#cat ./SKELS/maxmoc40.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/maxmoc40.py
#cat ./SKELS/gib.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/gib.py
#cat ./SKELS/nino.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/nino.py
#cat ./SKELS/cable.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/cable.py
cat ./SKELS/transports1.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/transports1.py
cat ./SKELS/mht1.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/mht1.py
cat ./SKELS/icemonth.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/icemonth.py
cat ./SKELS/icenoaa.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/icenoaa.py
cat ./SKELS/icetrd.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/icetrd.py
cat ./SKELS/tsmean.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/tsmean.py
#cat ./SKELS/tsmean_lev.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/tsmean_lev.py


cd $HOME


#python $RUNDIR/maxmoc.py
#python $RUNDIR/maxmoc40.py
#python $RUNDIR/gib.py
#python $RUNDIR/nino.py
#python $RUNDIR/cable.py
python $RUNDIR/transports1.py
python $RUNDIR/mht1.py
python $RUNDIR/icemonth.py
python $RUNDIR/icenoaa.py
python $RUNDIR/icetrd.py
python $RUNDIR/tsmean.py
#python $RUNDIR/tsmean_lev.py

##############################################################################################
#Put timeseries on MONITORING DRAKKAR website

ssh meolipc.hmg.inpg.fr -l apache \
" if [ ! -d web/DRAKKAR/${CONFIG} ] ; then mkdir web/DRAKKAR/${CONFIG} ; fi "
ssh meolipc.hmg.inpg.fr -l apache \
" if [ ! -d web/DRAKKAR/${CONFIG}/${CONFIG}-${CASE} ] ; then mkdir web/DRAKKAR/${CONFIG}/${CONFIG}-${CASE} ; fi "
ssh meolipc.hmg.inpg.fr -l apache \
" if [ ! -d web/DRAKKAR/${CONFIG}/${CONFIG}-${CASE}/TIME_SERIES ] ; then mkdir web/DRAKKAR/${CONFIG}/${CONFIG}-${CASE}/TIME_SERIES ; fi "

cd ${STOCKDIR}/${CONFIG}/PLOTS/${CONFIG}-${CASE}/TIME_SERIES/
for plot in *.png ; do
 scp $plot apache@meolipc.hmg.inpg.fr:web/DRAKKAR/${CONFIG}/${CONFIG}-${CASE}/TIME_SERIES/$plot
done
