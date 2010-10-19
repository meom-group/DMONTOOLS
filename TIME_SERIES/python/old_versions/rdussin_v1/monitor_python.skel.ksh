#!/bin/ksh

# $Id: monitor_python.skel.ksh 18 2009-02-05 18:31:08Z rdussin $ 
# $Date: 2009-02-05 19:31:08 +0100 (jeu. 05 févr. 2009) $
# $Rev: 18 $
#################################################################################
###                                                                           ###
###                   TIME SERIES FOR WEB SITE IN PYTHON                      ###
###                                                                           ###
#################################################################################

CONFIG=CCOONNFFIIGG
CASE=CCAASSEE

RUNDIR=${HOME}/MONITOR_PY/RUN_${CONFIG}-${CASE}/

if [ ! -d ${HOME}/${CONFIG} ] ; then mkdir ${HOME}/${CONFIG} ; fi
if [ ! -d ${HOME}/${CONFIG}/PLOTS ] ; then mkdir ${HOME}/${CONFIG}/PLOTS ; fi
if [ ! -d ${HOME}/${CONFIG}/PLOTS/${CONFIG}-${CASE} ] ; then mkdir ${HOME}/${CONFIG}/PLOTS/${CONFIG}-${CASE} ; fi
if [ ! -d ${HOME}/${CONFIG}/PLOTS/${CONFIG}-${CASE}/TIME_SERIES ] ; then mkdir ${HOME}/${CONFIG}/PLOTS/${CONFIG}-${CASE}/TIME_SERIES ; fi
if [ ! -d ${HOME}/MONITOR_PY/RUN_${CONFIG}-${CASE} ] ; then mkdir ${HOME}/MONITOR_PY/RUN_${CONFIG}-${CASE} ; fi

cat ./SKELS/maxmoc.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/maxmoc.py
cat ./SKELS/maxmoc40.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/maxmoc40.py
cat ./SKELS/gib.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/gib.py
cat ./SKELS/nino.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/nino.py
cat ./SKELS/cable.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/cable.py
cat ./SKELS/transports1.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/transports1.py
cat ./SKELS/mht1.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/mht1.py
cat ./SKELS/icemonth.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/icemonth.py
cat ./SKELS/icenoaa.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/icenoaa.py
cat ./SKELS/icetrd.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/icetrd.py
cat ./SKELS/tsmean.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/tsmean.py
cat ./SKELS/tsmean_lev.skel.py | sed -e "s/CCOONNFFIIGG/$CONFIG/g" -e "s/CCAASSEE/$CASE/g" > $RUNDIR/tsmean_lev.py


cd $HOME

python $RUNDIR/maxmoc.py
python $RUNDIR/maxmoc40.py
python $RUNDIR/gib.py
python $RUNDIR/nino.py
python $RUNDIR/cable.py
python $RUNDIR/transports1.py
python $RUNDIR/mht1.py
python $RUNDIR/icemonth.py
python $RUNDIR/icenoaa.py
python $RUNDIR/icetrd.py
python $RUNDIR/tsmean.py
python $RUNDIR/tsmean_lev.py


