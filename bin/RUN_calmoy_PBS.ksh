#!/bin/ksh
if [ $# = 0 ] ; then
   echo USAGE: RUN_calmoy.ksh  year
   exit 1
fi
set -x

# get configuration settings
. ./config_moy.ksh
. ./function_moy.ksh

# set additional variables
WPDIR=/work/$USER/RUN_${CONFIG}/${CONFIG}-${CASE}/CTL/CDF
chkdir /work/$USER/RUN_${CONFIG}/
chkdir /work/$USER/RUN_${CONFIG}/${CONFIG}-${CASE}/
chkdir /work/$USER/RUN_${CONFIG}/${CONFIG}-${CASE}/CTL/
chkdir $WPDIR


# copy required scripts there:
cp -f config_moy.ksh $WPDIR
cp -f function_moy.ksh $WPDIR

cp -f mvcalmoy.ksh $WPDIR
cp -f calannual.ksh $WPDIR

# get year to work with
year=$1

#prepare the 12 scripts in WPDIR

for m in $(seq 1 12 ) ; do
  mm=$(printf "%02d" $m )
  cat $MPDIR/calmoy_mm.ksh | sed -e "s/<year>/$year/g" -e "s/<mm>/$mm/g" > $WPDIR/zz_calmoy_${mm}_$year.ksh
  chmod 755 $WPDIR/zz_calmoy_${mm}_$year.ksh
done

# submit calmoy.pbs
cat $MPDIR/calmoy.pbs | sed -e "s/<CONFIG>/$CONFIG/" -e "s/<CASE>/$CASE/" -e "s/<year>/$year/g" > zcalmoy.pbs

jobidmoy=$(qsub zcalmoy.pbs)

\rm zcalmoy.pbs

#prepare the 12 scripts in WPDIR

for m in $(seq 1 12 ) ; do
  mm=$(printf "%02d" $m )
  cat $MPDIR/calmoyvt_mm.ksh | sed -e "s/<year>/$year/g" -e "s/<mm>/$mm/g" > $WPDIR/zz_calmoyvt_${mm}_$year.ksh
  chmod 755 $WPDIR/zz_calmoyvt_${mm}_$year.ksh
done

# submit calmoyvt.pbs
cat $MPDIR/calmoyvt.pbs | sed -e "s/<CONFIG>/$CONFIG/" -e "s/<CASE>/$CASE/" -e "s/<year>/$year/g" > zcalmoyvt.pbs
jobidvt=$(qsub zcalmoyvt.pbs)

\rm zcalmoyvt.pbs

# submit calanual script with dependencies afterok on jobidmoy and jobidvt

cat $MPDIR/calannual.pbs | sed -e "s/<CONFIG>/$CONFIG/" -e "s/<CASE>/$CASE/" \
     -e "s/<JOBIDMOY>/$jobidmoy/" -e  "s/<JOBIDVT>/$jobidvt/" -e "s/<year>/$year/g" > zcalannual_$year.pbs

qsub zcalannual_$year.pbs

